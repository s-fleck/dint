assert_lubridate <- function(x){
  if (!require_lubridate())
    stop(paste0(
      "This function requires the package 'lubridate'.",
      "You can install it via install.packages('lubridate')"
    ))
}




require_lubridate <- function(x){
  requireNamespace("lubridate", quietly = TRUE)
}





tz <- function(x){
  tzone <- attr(x, "tzone")[[1]]
  if (is.character(tzone) && nzchar(tzone)){
    tzone
  } else {
    "UTC"
  }
}




is_POSIXlt <- function(x){
  inherits(x, "POSIXlt")
}




make_date <- function(y, m, d){
  if (require_lubridate()){
    lubridate::make_date(y, m, d)
  } else {
    as.Date(ISOdate(y, m, d))
  }
}




substr_right <- function(x, n){
  nc <- nchar(x)
  substr(x, nc - n + 1, nc)
}
