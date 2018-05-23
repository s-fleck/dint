#' Get years component of a date_xx
#'
#' `year()` is there for consistency with [lubridate], `get_year()` is there
#' for consistency with other `get_` functions in \pkg{hammr}.
#'
#' @param x a [date_xx] object
#' @family date_xx getters
#'
#' @md
#' @export
#' @aliases year
#'
#' @examples
#'
#' \dontrun{
#' x <- date_yq(2016, 2)
#'
#' year(x)
#' get_year(x)
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
#' @rdname get_year
year.date_xx <- function(x){
  get_year(x)
}
