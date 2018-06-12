#' Get Components of a date_xx
#'
#' `get_year()` returns the year of a `date_xx` object.
#' A method for [lubridate::year()] is also exported. This will have
#' slightly more overhead than using `get_year()`, but should generally be
#' prefered for consistency in function naming across packages.
#'
#'
#' @param x a [date_xx] object
#'
#' @seealso [dint::year]
#' @export
#' @rdname getters
#'
#' @examples
#'
#' \dontrun{
#' x <- date_yq(2016, 2)
#'
#' year(x)
#'   get_year(x)
#' }
#'
get_year <- function(x){
  UseMethod("get_year")
}




#' @export
get_year.date_y <- function(x){
  x
}




#' @export
get_year.date_ym <- function(x){
  as.integer(x) %/% 100
}




#' @export
get_year.date_yq <- function(x){
  as.integer(x) %/% 10
}




#' @export
get_year.default <- function(x){
  as.POSIXlt(x, tz = tz(x))$year + 1900
}




#' Get Components of a date_xx (lubridate compat)
#'
#' See [lubridate::year()] and [lubridate::month()]
#'
#' @inheritParams get_year
#' @seealso [get_year]
#' @rdname year
#' @aliases month year
year.date_xx <- function(x){
  get_year(x)
}

