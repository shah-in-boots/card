<!-- README.md is generated from README.Rmd. Please edit that file -->

# autonomicR

<!-- badges: start -->

<!-- badges: end -->

The goal of autonomicR is to …

## Installation

Once published, you can install the released version of autonomicR from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("autonomicR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asshah4/autonomicR")
#> Skipping install of 'autonomicR' from a github remote, the SHA1 (4b74ea69) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(autonomicR)
#> Loading required package: magrittr
#> Loading required package: tidyverse
#> -- Attaching packages ---------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.0     v purrr   0.3.4
#> v tibble  3.0.1     v dplyr   0.8.5
#> v tidyr   1.0.2     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.5.0
#> -- Conflicts ------------------------- tidyverse_conflicts() --
#> x tidyr::extract()   masks magrittr::extract()
#> x dplyr::filter()    masks stats::filter()
#> x dplyr::lag()       masks stats::lag()
#> x purrr::set_names() masks magrittr::set_names()
#> Loading required package: data.table
#> 
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
#> The following object is masked from 'package:purrr':
#> 
#>     transpose
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="readme_files/figure-gfm/pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
