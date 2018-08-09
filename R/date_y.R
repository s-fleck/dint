#' A Simple Year Date Format
#'
#' A simple data type for storing years. A `date_y` object is just an
#' integer with an additional class atribute.
#'
#' @param y year
#'
#' @return `date_y` returns an object of type `date_y`
#' @export
#' @family simple dates
#' @md
#'
#' @examples
#' date_y(2013)
#'
date_y <- function(y){
  stopifnot(is.numeric(y) || all(is.na(y)))
  date_xx(as.integer(y), "date_y")
}




#' @param x any R object
#' @return `is_date_y` returns `TRUE` or `FALSE` depending on whether its
#'   argument is of type `date_y` or not.
#'
#' @export
#' @md
#' @rdname date_y
is_date_y <- function(x){
  inherits(x, "date_y")
}




# as_data_y ---------------------------------------------------------------

#' @md
#' @return `as_date_m` attempts to coerce its argument to `date_y` type
#' @export
#' @rdname date_y
#'
#' @examples
#' as_date_y(2016)
#'
as_date_y <- function(x){
  UseMethod("as_date_y")
}




#' @export
as_date_y.default <- function(x){
  as_date_y.Date(as.Date(x))
}




#' @export
as_date_y.numeric <- function(x){
  date_y(x)
}




#' @export
as_date_y.Date <- function(x){
  date_y(get_year(x))
}




# as.Date -----------------------------------------------------------------

#' @rdname as.Date.date_xx
#' @export
#'
as.Date.date_y <- function(
  x,
  ...
){
  make_date(x, 1, 1L)
}
