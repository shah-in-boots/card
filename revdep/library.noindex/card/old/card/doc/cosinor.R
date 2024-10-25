## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- setup-------------------------------------------------------------------
library(card)

# Using the provide dataset
data("twins")
head(twins)

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(twins, aes(x = hour, y = rDYX)) +
  geom_smooth(method = "gam")

## -----------------------------------------------------------------------------
# Model
m <- cosinor(rDYX ~ hour, data = twins, tau = 24)

# Summary
summary(m)

# Plot
ggcosinor(m, labels = TRUE)

## -----------------------------------------------------------------------------
# Model
m <- cosinor(rDYX ~ hour, data = twins, tau = c(24, 12))

# Summary
summary(m)

# Plot
ggcosinor(m, labels = TRUE)

## -----------------------------------------------------------------------------
# Model
m <- cosinor(rDYX ~ hour, data = twins, tau = c(24), population = "patid")

# Summary
summary(m)

