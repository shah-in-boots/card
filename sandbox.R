# Libraries for package development ====

# For exploring
library(devtools)
document()

library(tidyverse)
library(tidymodels)

# Problem with population cosinor
ecg <- readRDS("../twins/_targets/objects/ecg")
data <- ecg

# Labels
tau <- 24
population <- "patid"

processed <- hardhat::mold(dyx ~ hour, data)
population <- data[[population]]

