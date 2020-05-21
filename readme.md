
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/asshah4/card/workflows/R-CMD-check/badge.svg)](https://github.com/asshah4/card/actions)
[![Travis build
status](https://travis-ci.com/asshah4/card.svg?branch=master)](https://travis-ci.com/asshah4/card)
[![Github commit
frequency](https://img.shields.io/github/commit-activity/w/asshah4/card)](https://github.com/asshah4/card/graphs/commit-activity)
<!-- badges: end -->

# **card**

## Cardiovascular and Autonomic Research Design

The goal of **card** is to create functions and analytical approaches in
neurocardiology, with a focus on electrocardiogram data and
epidemiological concepts. An important engine within this ecosystem are
the circadian analysis tools. Its being developed to fit the *tidy*
approach to statistical analysis.

The areas of focus of this package are the following:

  - cardiovascular disease
  - circadian rhythms
  - electrocardiography (heart rate variability and morphology)
  - signal processing

## Installation

Once published, you can install the released version of **card** from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("card")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asshah4/card")
#> Skipping install of 'card' from a github remote, the SHA1 (bd978e39) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Usage

This package is intended for analyzing cardiovascular signals and
autonomic physiology. Here is an example of the process of fitting a
cosinor model to time-series data.

``` r
knitr::include_graphics("man/figures/cosinor-building-curves.jpeg")
```

<img src="man/figures/cosinor-building-curves.jpeg" width="100%" />