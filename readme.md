
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
#> Downloading GitHub repo asshah4/card@master
#> broom (5fc28ca50... -> a56ab063d...) [GitHub]
#> Downloading GitHub repo tidymodels/broom@master
#> 
#>      checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/RtmpwfflM5/remotes16b382a33e418/tidymodels-broom-a56ab06/DESCRIPTION’ ...  ✓  checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/RtmpwfflM5/remotes16b382a33e418/tidymodels-broom-a56ab06/DESCRIPTION’
#>   ─  preparing ‘broom’:
#>      checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts (387ms)
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘broom_0.7.0.9000.tar.gz’
#>      
#>      checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/RtmpwfflM5/remotes16b387d8cee7a/asshah4-card-eb6cd17/DESCRIPTION’ ...  ✓  checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/RtmpwfflM5/remotes16b387d8cee7a/asshah4-card-eb6cd17/DESCRIPTION’
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
autonomic physiology. Here is an example of the process of fitting a
cosinor model to time-series data.

``` r
knitr::include_graphics("man/figures/cosinor-building-curves.jpeg")
```

<img src="man/figures/cosinor-building-curves.jpeg" width="100%" />
