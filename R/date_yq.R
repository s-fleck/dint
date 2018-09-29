# ctor --------------------------------------------------------------------

#' A Simple S3-Class for Year-Quarter Dates
#'
#' A simple data type for storing year-quarter dates in a human readable integer
#' format, e.g.: 3.Quarter of 2012 is stored as 20123. Supports simple
#' arithmetic operations such as `+` and `-` as well formatting.
#'
#' @param y year
#' @param q quarter (optional)
#'
#' @return `date_yq` returns an object of type `date_yq`
#' @export
#' @family date_xx subclasses
#' @seealso [format.date_yq()], [seq.date_yq()], [date_xx_arithmetic()]
#'
#' @examples
#' date_yq(2013, 3)
#'
date_yq <- function(y, q) {
  stopifnot(
    is.numeric(y) || all(is.na(y)),
    is.numeric(q) || all(is.na(q)),
    all(q %in% c(1:4) | is.na(q))
  )

  s <- ifelse(sign(y) >= 0, 1L, -1L)
  res <- (as.integer(abs(y)) * 10L + as.integer(q)) * s

  date_xx(res, "date_yq")
}




# as_data_yq --------------------------------------------------------------

#' @param x any R object
#' @return `is_date_yq` returns `TRUE` or `FALSE`  depending on whether its
#'   argument is of type `date_yq` or not.
#'
#' @export
#' @rdname date_yq
is_date_yq <- function(x){
  inherits(x, "date_yq")
}




#' @return `as_date_yq` attempts to coerce its argument to `date_yq`
#' @export
#' @rdname date_yq
#'
#' @examples
#' as_date_yq(20161)
#'
as_date_yq <- function(x){
  UseMethod("as_date_yq")
}




#' @export
as_date_yq.date_yq <- function(x){
  x
}




#' @export
as_date_yq.default <- function(x){
  as_date_yq.Date(as.Date(x))
}




#' @export
as_date_yq.numeric <- function(x){
  stopifnot(all(x > 0 | x <= -11L))
  d <- yqs_matrix_from_numeric(x)
  date_yq(y = d[, 1] * d[, 3], q = d[, 2])
}




#' @export
as_date_yq.Date <- function(x){
  y <- get_year(x)
  q <- get_quarter(x)
  date_yq(y = y, q = q)
}




# as.Date -----------------------------------------------------------------

#' @rdname as.Date.date_xx
#' @export
#'
as.Date.date_yq <- function(
  x,
  ...
){
  make_date(
    get_year(x),
    c(1, 4, 7, 10)[get_quarter(x)],
    1L
  )
}




# utils -------------------------------------------------------------------

yqs_matrix_from_numeric <- function(x){
  x <- unclass(x)
  matrix(
    c(abs(x) %/% 10, q = abs(x) %% 10, s = as.integer(sign(x))),
    ncol = 3
  )
}
