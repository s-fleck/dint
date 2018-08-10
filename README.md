
<!-- README.md is generated from README.Rmd. Please edit that file -->
dint
====

[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

A toolkit to work with year-quarter and year-week Dates.

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

# creation
q <- date_yq(2014, 4)
m <- as_date_ym(201412)


# arithmetic operations
# quarters
q
#> 2014-Q4
q + 1
#> 2015-Q1
seq(q -2, q + 2)
#> 2014-Q2 2014-Q3 2014-Q4 2015-Q1 2015-Q2


# months
m
#> 2014-M12
m + 1
#> 2015-M01
seq(m -2, m + 2)
#> 2014-M10 2014-M11 2014-M12 2015-M01 2015-M02


# formatting
format(q)
#> [1] "2014-Q4"
format(q, "short")
#> [1] "2014.4"
format(q, "shorter")
#> [1] "14.4"
format(m)
#> [1] "2014-M12"


# get start and end of period
last_day_of_quarter(q)
#> [1] "2014-12-31"
first_day_of_quarter(q)
#> [1] "2014-10-01"
first_day_of_month(Sys.Date())
#> [1] "2018-08-01"
```
