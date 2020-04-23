
<!-- README.md is generated from README.Rmd. Please edit that file -->

# autonomicR

<!-- badges: start -->

<!-- badges: end -->

The goal of autonomicR is to provide research tools that have been built
to evaluate autonomic function and data, including time series,
longitudinal, and recurrent events

## Installation

Once this package is released and published on CRAN, you can install the
released version of autonomicR from [CRAN](https://CRAN.R-project.org)
with:

``` r
#install.packages("autonomicR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asshah4/autonomicR")
#> Downloading GitHub repo asshah4/autonomicR@master
#>          checking for file 'C:\Users\asshah4\AppData\Local\Temp\RtmpqGTbQK\remotes4a5c2d141bc0\asshah4-autonomicR-4b74ea6/DESCRIPTION' ...  v  checking for file 'C:\Users\asshah4\AppData\Local\Temp\RtmpqGTbQK\remotes4a5c2d141bc0\asshah4-autonomicR-4b74ea6/DESCRIPTION'
#>       -  preparing 'autonomicR':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   v  checking DESCRIPTION meta-information
#>   Warning:     Warning: C:/Users/asshah4/AppData/Local/Temp/RtmpKU9239/Rbuild49c6f741d6d/autonomicR/man/geom_forest.Rd:16: unknown macro '\item'
#>    Warning: C:/Users/asshah4/AppData/Local/Temp/RtmpKU9239/Rbuild49c6f741d6d/autonomicR/man/geom_forest.Rd:18: unexpected section header '\value'
#>    Warning: C:/Users/asshah4/AppData/Local/Temp/RtmpKU9239/Rbuild49c6f741d6d/autonomicR/man/geom_forest.Rd:21: unexpected section header '\description'
#>    Warning: C:/Users/asshah4/AppData/Local/Temp/RtmpKU9239/Rbuild49c6f741d6d/autonomicR/man/geom_forest.Rd:24: unexpected section header '\details'
#>    Warning: C:/Users/asshah4/AppData/Local/Temp/RtmpKU9239/Rbuild49c6f741d6d/autonomicR/man/geom_forest.Rd:27: unexpected section header '\examples'
#>    Warning: C:/Users/asshah4/AppData/Local/Temp/RtmpKU9239/Rbuild49c6f741d6d/autonomicR/man/geom_forest.Rd:30: unexpected END_OF_INPUT '
#>    '
#>       -  checking for LF line-endings in source and make files and shell scripts
#>       -  checking for empty or unneeded directories
#>       -  looking to see if a 'data/datalist' file should be added
#>       -  building 'autonomicR_0.1.91.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/asshah4/Documents/R/win-library/3.6'
#> (as 'lib' is unspecified)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(autonomicR)
#> Loading required package: magrittr
#> 
#> Attaching package: 'magrittr'
#> The following object is masked from 'package:purrr':
#> 
#>     set_names
#> The following object is masked from 'package:tidyr':
#> 
#>     extract
#> Loading required package: data.table
#> data.table 1.12.8 using 6 threads (see ?getDTthreads).  Latest news: r-datatable.com
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

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
