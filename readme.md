
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
[![Codecov test
coverage](https://codecov.io/gh/asshah4/card/branch/master/graph/badge.svg)](https://codecov.io/gh/asshah4/card?branch=master)
<!-- badges: end -->

# **card**

## Cardiovascular and Autonomic Research Design

The goal of **card** is to create functions and analytical approaches in
neurocardiology, with a focus on electrocardiogram data and
epidemiological concepts. An important engine within this ecosystem are
the circadian analysis tools. Its being developed to fit a *tidy*
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
#> Downloading GitHub repo asshah4/card@master
#> broom (e43db853a... -> 13ad2d321...) [GitHub]
#> Downloading GitHub repo tidymodels/broom@master
#> 
#>      checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/RtmpSGXs0v/remotesb7dc43b3ad34/tidymodels-broom-13ad2d3/DESCRIPTION’ ...  ✓  checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/RtmpSGXs0v/remotesb7dc43b3ad34/tidymodels-broom-13ad2d3/DESCRIPTION’
#>   ─  preparing ‘broom’:
#>      checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts (435ms)
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘broom_0.7.0.9000.tar.gz’
#>      
#>      checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/RtmpSGXs0v/remotesb7dcffbdb3e/asshah4-card-1dfad4b/DESCRIPTION’ ...  ✓  checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/RtmpSGXs0v/remotesb7dcffbdb3e/asshah4-card-1dfad4b/DESCRIPTION’
#>   ─  preparing ‘card’:
#>      checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>        NB: this package now depends on R (>= 3.5.0)
#>        WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects: ‘card/data/hrv.rda’
#> ─  building ‘card_0.1.0.tar.gz’
#>      
#> 
```

## Usage

This package is intended for analyzing cardiovascular signals and
autonomic physiology. The current collection of functions are built
practically around this paradigm. The current focus, in `card v0.1.0` is
the development of a flexible and tidy analytical approach to circadian
rhythms.

### Cosinor Analysis

Here is an example of the process of fitting a cosinor model to
time-series data.

``` r
library(card)
data(twins)
m <- cosinor(rDYX ~ hour, twins, tau = 24)
summary(m)
#> Individual Cosinor Model 
#> ------------------------------------------
#> Call: 
#> cosinor("rDYX ~ hour")
#> 
#> Residuals: 
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -3.12633 -0.53228 -0.03597  0.00000  0.49132  4.82150 
#> 
#> Coefficients: 
#>      mesor        amp        phi       beta      gamma 
#>  2.8604855  0.2986101 -2.6687044 -0.2658396  0.1360048
```

The data can also be easily plotted for diagnostic purposes.

``` r
ggcosinor(m)
#> `geom_smooth()` using formula 'y ~ s(x, bs = "cs")'
```

<img src="man/figures/unnamed-chunk-5-1.png" width="100%" />
