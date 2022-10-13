## Test environments
* ubuntu 18.04 (via RStudio Server), R 3.6.1
* ubuntu 16.04 (via travis), R 3.6.1
* win-builder (devel)
* rhub (rhub::check_for_cran())

## R CMD check results

0 errors | 0 warnings | 0 note

## submission 

Update `as.POSIXlt.date_xx()` and `as.POSIXct.date_xx()` to be consistent with
tzone behaviour of `as.POSIXct.Date()` and `as.POSIXlt.Date()` in R-devel
