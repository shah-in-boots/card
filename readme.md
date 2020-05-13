
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **card**: Cardiovascular and Autonomic Research Design

<!-- badges: start -->

[![Lifecycle:
Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
\[![Github: Last
Commit](https://img.shields.io/github/last-commit/asshah4/card)\]
\[![Github: Commit
Frequency](https://img.shields.io/github/commit-activity/w/asshah4/card)\]
<!-- badges: end -->

The goal of **card** is to create functions and analytical approaches in
neurocardiology, with a focus on electrocardiogram data and
epidemiological concepts. An important engine within this ecosystem are
the circadian analysis tools.

## Installation

Once published, you can install the released version of \_\_card\_\_from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("card")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asshah4/card")
#> Skipping install of 'card' from a github remote, the SHA1 (21e2eaa0) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Examples

This package is intended for analyzing cardiovascular signals and
autonomic physiology. For example, here is an analysis that detects
circadian rhythmicity of a regression.

``` r
knitr::include_graphics("./man/figures/cosinor-building-curves.jpeg")
```

<img src="./man/figures/cosinor-building-curves.jpeg" width="100%" />
