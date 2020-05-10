# Libraries for package development ====

library(tidyverse)
library(fs)
library(devtools)
library(usethis)
library(hardhat)
library(data.table)

# Using LM function ====

data("twins")
period <- 24

twins$cosh <- cos((2*pi*twins$hour)/period)
twins$sinh <- sin((2*pi*twins$hour)/period)

m <- lm(rDYX ~ hour, data = twins)
n <- lm(rDYX ~ cosh, data = twins)
o <- lm(rDYX ~ sinh, data = twins)
p <- lm(rDYX ~ cosh + sinh, data = twins)

ggplot(twins, aes(x = hour, y = rDYX)) +
	geom_point(alpha = 0.2, colour = "grey") +
	geom_line(aes(y = m$fitted.values), colour = "black") +
	geom_line(aes(y = n$fitted.values), colour = "blue") +
	geom_line(aes(y = o$fitted.values), colour = "red") +
	geom_line(aes(y = p$fitted.values), colour = "purple") +
	theme_minimal() +
	labs(
		x = "Hours",
		y = "DYX",
		title = "Model building: Y = B1*cos(hr) + B2*sin(hr) + intercept",
		caption = "Black = linear, Red = sin, Blue = cos, Purple = sin+cos"
	) +
	ylim(2.5,3.3)

ggplot(m, aes(x = hour, y = rDYX)) +
	geom_smooth(method = "lm", se = TRUE)

# Molding
data("twins")

# Formula
processed <- mold(rDYX ~ hour, twins)

# Data frame
processed <- mold(x = twins["hour"], y = twins["rDYX"])

# Matrix
processed <- mold(x = as.matrix(twins$hour), y = twins$rDYX, blueprint = blueprint)

# Recipe
rec <- recipe(rDYX ~ hour, twins) %>%
	step_center(all_predictors())
processed <- mold(rec, twins)

# Using normal equations ====

period <- 24
t <- twins$hour
y <- twins$rDYX

# Formal equation
# y(t) = M + beta*x + gamma*z + error(t)
	# beta = A*cos(phi)
	# gamma = -A*sin(phi)
	# x = cos(2*pi*t/period)
	# z = sin(2*pi*t/period)
	# M = MESOR
	# A = Amplitude
	# phi = acrophase (measure of hte time of overall high values in cycle)

# RSS = sum[y - (M + beta*x + gamma*z)]^2

# Normal equations (where M, beta, gamma are the coefficients to solve for)
	# sum(y) = M*n + beta*sum(x) + gamma*sum(z)
	# sum(y*x) = M*sum(x) + beta*sum(x^2) + gamma*sum(x*z)
	# sum(y*z) = M*sum(z) + beta*sum(x*z) + gamma*sum(z^2)

# Parameters
n <- length(t)
x <- cos((2 * pi * t)/period)
z <- sin((2 * pi * t)/period)

# Linear columns

# Matrices
ymat <- as.matrix(cbind(y = c(sum(y), sum(y*x), sum(y*z))))
mcol <- c(n, sum(x), sum(z))
bcol <- c(sum(x), sum(x^2), sum(x*z))
gcol <- c(sum(z), sum(x*z), sum(z^2))
xmat <- as.matrix(cbind(mesor = mcol, beta = bcol, gamma = gcol))

# Solve linear equations
coef <- solve(t(xmat) %*% xmat) %*% (t(xmat) %*% ymat)
mesor <- coef[1] # mesor
beta <- coef[2]  # beta
gamma <- coef[3] # gamma

# Amplitude
amp <- sqrt(b^2 + g^2)

# Acrophase (phi) must be in correct quadrant
sb <- sign(beta)
sg <- sign(gamma)
theta <- atan(abs(gamma/beta))

if((sb == 1 | sb == 0) & sg == 1) {
	phi = -theta
} else if(sb == -1 & (sg == 1 | sg == 0)) {
	phi = theta - pi
} else if((sb == -1 | sb == 0) & sg == -1) {
	phi = -theta - pi
} else if(sb == 1 & (sg == -1 | sg == 0)) {
	phi = theta - (2*pi)
}

### MODEL FITTING ###

# Predicted values
yhat <- mesor + amp*cos((2*pi*t)/period + phi)
yhat <- mesor + beta*x + gamma*z

# Square differences
	# Total sum of squares = model sum of squares + residual sum of squares
	# TSS = MSS + RSS
	# sum(y - ymean)^2 = sum(yhat - ymean)^2 + sum(y - yhat)^2
TSS <- sum((y - mean(y))^2)
MSS <- sum((yhat - mean(y))^2)
RSS <- sum((y - yhat)^2)

# F-test
Fstat <- (MSS/2) / (RSS/(n-3))

if(TSS == MSS + RSS) {
	paste0("The model fits. F = ", signif(Fstat, 4))
}

### MESOR ###

# Set alpha
alpha <- 0.05

# Matrix to get standard errors and confidence intervals
s <- 1/xmat

# MESOR parameter estimates
sigma <- sqrt(RSS/(n-3))
tdist <- qt(1 - alpha/2, df = n - 3)
ciM <- tdist * sigma * sqrt(s[1,1])
ciMesorMax <- mesor + ciM
ciMesorMin <- mesor - ciM

# Not sure what these are yet...
xc <- 1/n * sum((x - mean(x))^2)
zc <- 1/n * sum((z - mean(z))^2)
tc <- 1/n * sum((x - mean(x)) * (z - mean(z)))

# CI for MESOR ( why is sigma squared)
ciM <- tdist * sigma^2 * sqrt(
	((sum(x^2))*(sum(z^2)) - ((sum(x*z))^2)) / (n^3 * (xc*zc - tc^2))
)
ciMesorMax <- mesor + ciM
ciMesorMin <- mesor - ciM

### beta / gamma confidence region ###

# Amplitude standard error (not working)
sigma^2 * sqrt(s[2,2]*cos(phi)^2  - 2*s[2,3]*sin(phi)*cos(phi)  - s[3,3]*sin(phi)^2)

# Find beta and gamma CI region
fdist <- qf(1 - alpha/2, df1 = 2, df2 = n-3)

# Quadratic formula setup
a <- xc
b <- 2*tc
c <- zc
d <- -2*xc*beta - 2*tc*gamma
e <- -2*tc*beta - 2*zc*gamma
f <- xc*beta^2 + 2*tc*beta*gamma + zc*gamma^2 - (2/n)*sigma^2*fdist
gmax <- -(2*a*e - d*b) / (4*a*c - b^2)

# Identify parameters for ellipses
gseq <- seq(from = gmax-amp*2, to = gmax+amp*2, by = amp/1000)
bs1 <- (-(b*gseq + d) + sqrt(
	as.complex((b*gseq + d)^2 - 4*a*(c*gseq^2 + e*gseq + f))
)) / (2*a)
bs2 <- (-(b*gseq + d) - sqrt(
	as.complex((b*gseq + d)^2 - 4*a*(c*gseq^2 + e*gseq + f))
)) / (2*a)

# Isolate the elliptical region (non imaginary)
index <- which(Re(bs1) != Re(bs2))
gseq <- gseq[index]
bs1 <- Re(bs1[index])
bs2 <- Re(bs2[index])

# Determine if confidence regions overlap the pole (if overlap, cannot get CI)
if(
	(diff(range(gseq)) >= max(gseq)) &
	((diff(range(bs1)) >= max(bs1)) | (diff(range(bs2)) >= max(bs2)))
) {
	print("Confidence regions overlap the poles. Confidence intervals for amplitude and acrophase cannot be determined.")
} else {
	# CI for Amplitude
	ciAmpMax <- max(c(sqrt(bs1^2 + gseq^2), sqrt(bs2^2 + gseq^2)))
	ciAmpMin <- min(c(sqrt(bs1^2 + gseq^2), sqrt(bs2^2 + gseq^2)))

	# CI for Acrophase
	theta <- c(atan(abs(gseq/bs1)), atan(abs(gseq/bs2)))
	sa <- sign(c(bs1, bs2))
	sb <- sign(c(gseq, gseq)) * 3
	sc <- sa + sb
	tmp <- sc
	ciPhi <- vector(mode = "double", length = length(theta))

	# Place theta in correct quadrant for phi
	for(i in 1:length(sc)) {
		if(sc[i] == 4 | sc[i] == 3) {
			ciPhi[i] <- -theta[i]
			sc[i] <- 1
		} else if(sc[i] == 2 || sc[i] == -1) {
			ciPhi[i] <- -pi + theta[i]
			sc[i] <- 2
		} else if(sc[i] == -4 || sc[i] == -3) {
			ciPhi[i] <- -pi - theta[i]
			sc[i] <- 3
		} else if(sc[i] == -2 || sc[i] == -1) {
			ciPhi[i] <- -2*pi + theta[i]
			sc[i] <- 4
		}
	}

	# Get max and min values for phi / acrophase
	if(max(sc) - min(sc) == 3) {
		ciPhiMax <- min(ciPhi[sc == 1])
		ciPhiMin <- max(ciPhi[sc == 4])
	} else {
		ciPhiMax <- max(ciPhi)
		ciPhiMin <- min(ciPhi)
	}

}

### PLOTTING ELLIPSE
range(gseq)
range(bs1)
range(bs2)
gamma
beta

plot(0, type = "n", xlim = c(-2.5 * amp, 2.5*amp), ylim = c(-2.5 * amp, 2.5*amp))
lines(gseq, bs1, col = "blue")
lines(gseq, bs2, col = "blue")
lines(c(0, gamma), c(0, beta), col = "black", lty = "dashed")
lines(c(gamma, -2 * amp * sin(phi)), c(beta, 2 * amp * cos(phi)), col = "red")
lines(clock[,1], clock[,2], col = "grey")

theta_clock <- seq(0, 2*pi, length.out = 24^2)
clock <- cbind(2*amp*cos(theta_clock), 2*amp*sin(theta_clock))

rad <- seq(0, 2*pi-pi/4, by = pi/4)
rad_clock <- cbind(2.2*amp*cos(rad), 2.2*amp*sin(rad))




ggplot() +
	xlim(-2.5 * amp, 2.5*amp) +
	ylim(-2.5 * amp, 2.5*amp) +
	theme_minimal() +
	geom_line(aes(x = gseq, y = bs1), col = "cornflowerblue", size = 1) +
	geom_line(aes(x = gseq, y = bs2), col = "cornflowerblue", size = 1) +
	geom_line(aes(
		x = c(0, gamma),
		y = c(0, beta)
	), lty = 1, size = 1, col = "grey") +
	geom_line(aes(
		x = c(gamma, -2 * amp * sin(phi)),
		y = c(beta, 2 * amp * cos(phi)),
		group = 0
	), size = 1, col = "indianred4", lty = 3) +
	geom_line(aes(x = c(0,0), y = c(-2*amp, 2*amp)), lty = 5) +
	geom_line(aes(y = c(0,0), x = c(-2*amp, 2*amp)), lty = 5) +
	xlab(expression(paste(gamma))) +
	ylab(expression(paste(beta))) +
	geom_path(aes(x = clock[,1], y = clock[,2]), col = "grey") +
	geom_path(aes(x = 1.2*clock[,1], y = 1.2*clock[,2]), col = "grey") +
	annotate(
		geom = "text", x = rad_clock[,1], y = rad_clock[,2],
		label = paste(rad*180/pi, "\u00B0")
	)



