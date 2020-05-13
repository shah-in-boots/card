
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **card**

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
\[![Github last
commit](https://img.shields.io/github/last-commit/asshah4/card)\]
\[![Github commit
frequency](https://img.shields.io/github/commit-activity/w/asshah4/card)\]
[![Travis build
status](https://travis-ci.com/asshah4/card.svg?branch=master)](https://travis-ci.com/asshah4/card)
<!-- badges: end -->

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
#> glue (1.4.0 -> 1.4.1) [CRAN]
#> Installing 1 packages: glue
#> 
#>   There is a binary version available but the source version is later:
#>      binary source needs_compilation
#> glue  1.4.0  1.4.1              TRUE
#> installing the source package 'glue'
#>      checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/RtmpHFFhXt/remotes1255b349c6dc4/asshah4-card-28bd05a/DESCRIPTION’ ...  ✓  checking for file ‘/private/var/folders/_6/4nlf2_lj735c3cxjs7_chblr0000gr/T/RtmpHFFhXt/remotes1255b349c6dc4/asshah4-card-28bd05a/DESCRIPTION’
#>   ─  preparing ‘card’:
#>    checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘card_0.1.0.tar.gz’
#>      
#> 
```

## Usage

This package is intended for analyzing cardiovascular signals and
autonomic physiology. For example, **card** can provide an analysis that
detects circadian rhythmicity of a regression.
