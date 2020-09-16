# Libraries for package development ====

# For admin
library(devtools)
library(hardhat)
library(pkgdown)
library(testthat)

# For exploring
library(tidyverse)
library(tidymodels)
library(data.table)

# Test out parsnip
data(twins)
split <- initial_split(twins, prop = 3/4)
train <- training(split)
test <- testing(split)

cosinor_mod <-
	cosinor_reg(period = 24) %>%
	set_engine("card") %>%
	set_mode("regression")

cosinor_fit <-
	cosinor_mod %>%
	fit(rDYX ~ hour, data = train)

