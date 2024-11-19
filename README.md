
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dint

[![CRAN
status](https://www.r-pkg.org/badges/version/dint)](https://cran.r-project.org/package=dint)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

A Toolkit for Year-Quarter, Year-Month and Year-Isoweek Dates

S3 classes and methods to create and work with year-quarter and
year-month vectors. Basic arithmetic operations (such as adding and
subtracting) are supported, as well as formatting and converting to and
from standard R Date types. For more info please refer to the [package
vignette](https://CRAN.R-project.org/package=dint/vignettes/dint.html)
or the [documentation](https://s-fleck.github.io/dint/)

## Dependencies

dint is implemented strictly in base R and will **always stay free of
hard dependencies**. The optional dependencies of dint are just there to
facilitate interoperability these packages if you are already using
them; for example by providing ggplot2 scales.

## Installation

Install the release version of dint from CRAN:

``` r
install.packages("dint")
```

Or install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("s-fleck/dint")
```

## Example

``` r
library(dint)

# creation
q <- date_yq(2014, 4)
m <- as_date_ym(201412)
w <- as_date_yw(as.Date("2017-01-01"))

# printing
print(q)
#> [1] "2014-Q4"
print(m)
#> [1] "2014-M12"
print(w)  # isoweeks do not follow calendar years!
#> [1] "2016-W52"

# arithmetic operations
# quarters
q
#> [1] "2014-Q4"
q + 1
#> [1] "2015-Q1"
seq(q -2, q + 2)
#> [1] "2014-Q2" "2014-Q3" "2014-Q4" "2015-Q1" "2015-Q2"


# months
m
#> [1] "2014-M12"
m + 1
#> [1] "2015-M01"
seq(m -2, m + 2)
#> [1] "2014-M10" "2014-M11" "2014-M12" "2015-M01" "2015-M02"


# formatting
format(q)
#> [1] "2014-Q4"
format(q, "%Y.%q")
#> [1] "2014.4"
format(q, "%y.%q")
#> [1] "14.4"
format(m)
#> [1] "2014-M12"


# get start and end of period
last_of_quarter(q)
#> [1] "2014-12-31"
first_of_quarter(q)
#> [1] "2014-10-01"
first_of_month(Sys.Date())
#> [1] "2024-11-01"
first_of_isoweek(w)
#> [1] "2016-12-26"
last_of_isoweek(w)
#> [1] "2017-01-01"
```
