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
  assert_that(is.numeric(y) || all(is.na(y)))
  date_xx(as.integer(y), 'date_y')
}




#' @param x any R object
#' @return `is_date_y` returns `TRUE` or `FALSE` depending on whether its
#'   argument is of type `date_y` or not.
#'
#' @export
#' @md
#' @rdname date_y
is_date_y <- function(x){
  inherits(x, 'date_y')
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
  UseMethod('as_date_y')
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
  date_y(lubridate::year(x))
}




# as.Date -----------------------------------------------------------------

#' Convert date_y to Date
#'
#' Returns the first day of the year
#'
#' @param x a [date_y] object
#' @param ... ignored
#'
#' @return A [base::Date] object
#' @md
#' @export
#'
as.Date.date_y <- function(x, ...){
  lubridate::make_date(x, 1, 1L)
}




# accessors ---------------------------------------------------------------

#' @export
get_year.date_y <- function(x){
  x
}




#' @export
get_quarter.date_y <- function(x){
  stop('Not supported for date_y objects')
}




#' @export
get_month.date_y <- function(x){
  stop('Not supported for date_y objects')
}
