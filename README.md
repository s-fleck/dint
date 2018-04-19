
<!-- README.md is generated from README.Rmd. Please edit that file -->
dint
====

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

Functions and classes for to work with integer years, year-quarter and year-week Dates.

Installation
------------

You can install dint from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("s-fleck/dint")
```

Example
-------

``` r

library(dint)

x <- as_date_yq(20154)

format(x)
#> [1] "2015-Q4"
format(x, "short")
#> [1] "2015.4"
format(x, "shorter")
#> [1] "15.4"

x + 1
#> [1] 20161
#> attr(,"class")
#> [1] "date_yq" "date_xx" "integer"

last_day_of_quarter(x)
#> [1] "2015-12-31"
first_day_of_quarter(x)
#> [1] "2015-10-01"
```
