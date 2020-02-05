## Test environments
* ubuntu 18.04 (via RStudio Server), R 3.6.1
* ubuntu 16.04 (via travis), R 3.6.1
* win-builder (devel)
* rhub (rhub::check_for_cran())

## R CMD check results

0 errors | 0 warnings | 0 note

## submission 

Correct a small error in the documentation of `date_xx_arithmetic` that
triggers a CRAN Warning since the fixing of 
https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16223>
