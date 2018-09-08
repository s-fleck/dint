# ctor --------------------------------------------------------------------

#' A Simple S3-Class for Year-Isoweek Dates
#'
#' A simple data type for storing year-isoweek dates in a human readable integer
#' format, e.g.: December 2012 is stored as 201212. Supports simple arithmetics
#' (`+` and `-`) as well formatting.
#'
#' @param y year
#' @param w week (optional)
#'
#' @return `date_yw` returns an object of type `date_yw`
#' @export
#' @family simple dates
#' @seealso [format.date_yw()]
#'
#' @examples
#' date_yw(2013, 12)
#'
date_yw <- function(y, w) {
  stopifnot(
    is.numeric(y) || all(is.na(y)),
    is.numeric(w) || all(is.na(w)),
    all(w %in% c(1:53) | is.na(w))
  )

  s <- ifelse(sign(y) >= 0, 1L, -1L)
  res <- (as.integer(abs(y)) * 100L + as.integer(w)) * s

  date_xx(res, "date_yw")
}




# as_data_yw --------------------------------------------------------------

#' @param x any R object
#' @return `is_date_yw` returns `TRUE` or `FALSE` depending on whether its
#'   argument is of type `date_yw` or not.
#'
#' @export
#' @rdname date_yw
is_date_yw <- function(x){
  inherits(x, "date_yw")
}




#' @return `as_date_yw` attempts to coerce its argument to `date_yw` type
#' @export
#' @rdname date_yw
#'
#' @examples
#' as_date_yw(201612)
#'
as_date_yw <- function(x){
  UseMethod("as_date_yw")
}




#' @export
as_date_yw.date_yw <- function(x){
  x
}




#' @export
as_date_yw.default <- function(x){
  as_date_yw.Date(as.Date(x))
}




#' @export
as_date_yw.numeric <- function(x){
  stopifnot(all(x > 0 | x <= -101L))
  d <- yws_matrix_from_numeric(x)
  date_yw(y = d[, 1] * d[, 3], w = d[, 2])
}




#' @export
as_date_yw.Date <- function(x){
  x <- as.POSIXlt(x)

  date <- make_date(get_year(x), get_month(x), x$mday)
  wday <- ifelse(x$wday == 0, 7L, x$wday)
  date <- date + (4L - wday)
  jan1 <- make_date(get_year(date), 1, 1)

  date_yw(
    y = get_year(jan1),
    w = 1L + (as.numeric(date) - as.numeric(jan1)) %/% 7L
  )
}




# as.Date -----------------------------------------------------------------

#' @rdname as.Date.date_xx
#' @export
#'
as.Date.date_yw <- function(x, ...){
  isoyear <- as.integer(x) %/% 100
  isoweek <- as.integer(x) %%  100

  jan1 <- as.POSIXlt(make_date(isoyear, 1, 1))
  jan1wday <- ifelse(jan1$wday == 0, 7L, jan1$wday)

  jan1 + (4L - jan1wday) + isoweek * 7L
}




# utils -------------------------------------------------------------------

yws_matrix_from_numeric <- yms_matrix_from_numeric
