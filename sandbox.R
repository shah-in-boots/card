# Libraries for package development ====

# For exploring
library(tidyverse)
library(tidymodels)
library(devtools)
load_all()

# Problem with population cosinor
ecg <- readRDS("../twins/_targets/objects/ecg")
data <- ecg

# Labels
tau <- 24
population <- "patid"

processed <- hardhat::mold(dyx ~ hour, data)
population <- data[[population]]

