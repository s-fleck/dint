#' Get Year, Quarter or Month
#'
#'
#' @details
#' If you use lubridate in addition to dint,
#' you can also use [lubridate::year()], [lubridate::month()] and
#' [lubridate::quarter()] with dint objects.
#'
#' @param x a [date_xx] or any \R object that can be coerced to `POSIXlt`
#'
#' @export
#' @rdname getters
#' @return
#'   an `integer` vector.
#'
#' @seealso
#'   [lubridate::year()],
#'   [lubridate::month()],
#'   [lubridate::quarter()]
#'
#' @examples
#' x <- date_yq(2016, 2)
#' get_year(x)
#' \dontrun{
#' library(lubridate)
#' year(x)
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
  as.integer(x) %/% 100L
}




#' @export
get_year.date_yq <- function(x){
  as.integer(x) %/% 10L
}




#' @export
get_year.default <- function(x){
  as.POSIXlt(x, tz = tz(x))$year + 1900L
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

