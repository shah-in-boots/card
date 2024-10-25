## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- setup-------------------------------------------------------------------
library(card)
library(magrittr)
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
# Data set to be used is included
data("twins")
head(twins)
df <- 
  twins %>%
  subset(patid %in% c(1:30))

## -----------------------------------------------------------------------------
# Zipcodes, contained as characters (because of leading 0)
data("zipcode")
head(zipcode)

# Get the zipcodes merged into to get latitude and longitude
df <- left_join(df, zipcode[c("zip", "latitude", "longitude")], by = "zip")

# Sunrise is dependent on location and date
df$sunrise <- circ_sun(date = df$date, lat = df$latitude, lon = df$longitude)

## -----------------------------------------------------------------------------
## Time series data
length(df$dyxtime)

# Number of participants
length(unique(df$patid))

# Unique sunrise times per patient
zeit <- 
  df %>%
  group_by(patid) %>%
  arrange(dyxtime) %>%
  select(patid, sunrise) %>%
  unique() %>%
  group_by(patid) %>%
  slice(n()) # Sunrise time during study
names(zeit)[2] <- "zeit"
  

# Add surnsie zeitgeiber back in
df %<>% left_join(., zeit, by = "patid")

x <- df %>%
  group_by(patid) %>%
  tidyr::nest()

# Slow and steady method for going through all the potential vectors
# Will look to "tidy" this in the future (TODO)
for(i in seq(x$patid)) {
  z <- unique(x[[i,2]][[1]]$zeit)
  t <- x[[i,2]][[1]]$dyxtime
  x[[i,2]][[1]]$zvec <- circ_center(x[[i,2]][[1]]$dyxtime, z)
}

# Visualize data trend
df <- tidyr::unnest(x)
summary(df$zvec)

# Pseudo-rose plot
ggplot(df, aes(x = hour, y = rDYX, group = hour, fill = hour)) + 
  geom_boxplot() +   
  coord_polar(theta = "x", start = 0, direction = 1) + 
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 24, 1)) + 
  scale_fill_viridis_c() + 
  theme_minimal()

