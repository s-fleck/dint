#' Get month of a date_xx
#'
#' `get_month()` returns the month of a `date_xx` object.
#' A method for [lubridate::month()] is also exported. This will have
#' slightly more overhead than using `get_month()`, but should generally be
#' prefered for consistency in function naming across packages.
#'
#' @param x a [date_xx] object.
#' @inheritParams lubridate::month
#' @rdname get_month
#' @aliases month
#' @seealso [lubridate::month()]
#' @family yq getters
#' @export
#'
#' @examples
#'
#' x <- date_yq(2016, 2)
#' get_month(x)
#'
#' \dontrun{
#' library(lubridate)
#' month(x)
#' month(x, label = TRUE)
#' }
#'
get_month <- function(x){
  UseMethod("get_month")
}




#' @export
get_month.date_y <- function(x){
  stop("Not supported for date_y objects")
}




#' @export
get_month.date_ym <- function(x){
  as.integer(as.integer(x) %% 100)
}




#' @export
get_month.date_yq <- function(x){
  c(1L, 4L, 7L, 10L)[get_quarter(x)]
}




#' @export
#' @rdname get_month
month.date_xx <- function(
  x,
  label = FALSE,
  abbr = TRUE,
  locale = Sys.getlocale("LC_TIME")
){
  assert_lubridate()
  lubridate::month(get_month(x), label = label, abbr = abbr, locale = locale)
}
