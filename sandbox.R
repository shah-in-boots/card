# Libraries for package development ====

# For exploring
library(tidyverse)
library(tidymodels)
library(devtools)
load_all()

# Testing out GEE
library(multilevelmod)
library(magrittr)
linear_reg() %>%
	set_engine("gee", corstr = "independence") %>%
	fit(mpg ~ wt + id_var(cyl), data = mtcars)

# For our research efforts
data(geh)
f <- qrs_tang ~ lab_hba1c + id_var(hhp_id)
x <-
	linear_reg() %>%
	set_engine("gee", corstr = "independence") %>%
	fit(f, data = geh)

y <- x$fit
