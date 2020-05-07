### Cosinor implementation
cosinor_impl <- function(predictors, outcome) {

	# Parameters for normal equations {{{ ====

  # Period of 24 hours in a day
  period <- 24
  alpha <- 0.05

  # Time variables (as predictors is just a variable for hours)
  # Needs to be shaped as a matrix for regression
  #tcos <- cos((2 * pi * predictors) / period) %>% as.matrix()
  #colnames(tcos) <- "tcos"
  #tsin <- sin((2 * pi * predictors) / period) %>% as.matrix()
  #colnames(tsin) <- "tsin"

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

	# Parameters using the predictors (time) and outcomes (y)
	y <- outcomes
  t <- predictors
	n <- length(t)
	x <- cos((2 * pi * t)/period)
	z <- sin((2 * pi * t)/period)

	# Matrices
	ymat <- as.matrix(cbind(y = c(sum(y), sum(y*x), sum(y*z))))
	mcol <- c(n, sum(x), sum(z)) # Mesor column
	bcol <- c(sum(x), sum(x^2), sum(x*z)) # Beta column
	gcol <- c(sum(z), sum(x*z), sum(z^2)) # Gamma column
	xmat <- as.matrix(cbind(mesor = mcol, beta = bcol, gamma = gcol))

	# }}}

	# Solve System of Equations {{{ ====
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
		phi <- -theta
	} else if(sb == -1 & (sg == 1 | sg == 0)) {
		phi <- theta - pi
	} else if((sb == -1 | sb == 0) & sg == -1) {
		phi <- -theta - pi
	} else if(sb == 1 & (sg == -1 | sg == 0)) {
		phi <- theta - (2*pi)
	}

	# }}}

	# Model Fit {{{ ====

	# Predicted values
	yhat <- mesor + beta*x + gamma*z

	# Square differences
		# Total sum of squares = model sum of squares + residual sum of squares
		# TSS = MSS + RSS
		# sum(y - ymean)^2 = sum(yhat - ymean)^2 + sum(y - yhat)^2
	TSS <- sum((y - mean(y))^2)
	MSS <- sum((yhat - mean(y))^2)
	RSS <- sum((y - yhat)^2)

	# F-test
	fstat <- (MSS/2) / (RSS/(n-3))

	if(TSS == MSS + RSS) {
		paste0("The model fits. F = ", signif(fstat, 4))
	}

	# }}}

	# Confidence Intervals for the MESOR {{{ ====

	# Matrix to get standard errors and confidence intervals
	s <- 1/xmat

	# MESOR parameter estimates
	sigma <- sqrt(RSS/(n-3))
	tdist <- qt(1 - alpha/2, df = n - 3)
	ciM <- tdist * sigma * sqrt(s[1,1])
	ciMesorMax <- mesor + ciM
	ciMesorMin <- mesor - ciM

	# }}}

	# Confidence Intervals for Amplitude and Acrophase {{{ ====

	# Correction of sum of squares for each parameter
	xc <- 1/n * sum((x - mean(x))^2)
	zc <- 1/n * sum((z - mean(z))^2)
	tc <- 1/n * sum((x - mean(x)) * (z - mean(z)))

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

	# Determine if ellipse regions overlap the pole (if overlap, cannot get CI)
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

	# }}}

  # Model Output {{{ ====

	# Model findings
	coefmat <- cbind(
		estimate = c(mesor, amp, phi),
		lowerCI = c(ciMesorMin, ciAmpMin, ciPhiMin),
		upperCI = c(ciMesorMax, ciAmpMax, ciPhiMax)
	)
	rownames(coefmat) <- c("mesor", "amp", "phi")

	# Model coefficients
	coefs <- c(mesor, amp, phi)
	names(coefs) <- c("MESOR", "Amplitude", "Acrophase")

  # Coef names
  coef_names <- names(coefs)
  coefs <- unname(coefs)

  # List to return
  list(
    coefs = coefs,
    coef_names = coef_names
  )

  # }}}

}




