
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/asshah4/card/workflows/R-CMD-check/badge.svg)](https://github.com/asshah4/card/actions)
[![Travis build
status](https://travis-ci.com/asshah4/card.svg?branch=master)](https://travis-ci.com/asshah4/card)
\[![Github last
commit](https://img.shields.io/github/last-commit/asshah4/card)\]
![Github commit
frequency](https://img.shields.io/github/commit-activity/w/asshah4/card)\](<https://github.com/asshah4/card/graphs/commit-activity>)
<!-- badges: end -->

# **card**

## Cardiovascular and Autonomic Research Design

The goal of **card** is to create functions and analytical approaches in
neurocardiology, with a focus on electrocardiogram data and
epidemiological concepts. An important engine within this ecosystem are
the circadian analysis tools.

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
#> 
#>      checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/Rtmp8MQrJn/remotese0734ee6c335/asshah4-card-0b557a6/DESCRIPTION’ ...  ✓  checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/Rtmp8MQrJn/remotese0734ee6c335/asshah4-card-0b557a6/DESCRIPTION’
#>   ─  preparing ‘card’:
#>      checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
#>   ─  excluding invalid files
#>      Subdirectory 'R' contains invalid file names:
#>      ‘NEWS.md’
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>        NB: this package now depends on R (>= 3.5.0)
#>      WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects: ‘card/data/hrv.rda’
#> ─  building ‘card_0.1.0.tar.gz’
#>      
#> 
```

## Usage

This package is intended for analyzing cardiovascular signals and
autonomic physiology. For example, **card** can provide an analysis that
detects circadian rhythmicity of a regression.

``` r
knitr::include_graphics("./man/figures/cosinor-building-curves.jpeg")
```

<img src="./man/figures/cosinor-building-curves.jpeg" width="100%" />
