# Libraries for package development ====

# For exploring
library(tidyverse)
library(tidymodels)
library(devtools)
load_all()

# Problem with population cosinor
ecg <- readRDS("../twins/_targets/objects/ecg")
data <- ecg

# Use the template from cosinor to set everything up
