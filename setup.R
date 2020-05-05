# Libraries for package development ====

library(tidyverse)
library(fs)
library(devtools)
library(rmarkdown)
library(knitr)
library(usethis)
library(testthat)
library(hardhat)
library(data.table)


# Scratch ====

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
procForm <- mold(rDYX ~ hour, twins)

# Data frame
procDf <- mold(x = twins["hour"], y = twins["rDYX"])

# Matrix
procMat <- mold(x = as.matrix(twins$hour), y = twins$rDYX, blueprint = blueprint)

# Recipe
rec <- recipe(rDYX ~ hour, twins) %>%
	step_center(all_predictors())
procRec <- mold(rec, twins)

# processed
processed <- procMat
