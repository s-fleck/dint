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




year <- function(x){
  UseMethod("year")
}




year.default <- function(x){
  as.POSIXlt(x, tz = tz(x))$year + 1900
}




quarter <- function(x) {
  quarters <- rep(1L:4L, each = 3L)
  quarters[match(month(x), 1L:12L)]
}




month <- function(
  x
){
  as.POSIXlt(x, tz = tz(x))$mon + 1L
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
