# ctor --------------------------------------------------------------------

#' A Simple S3-Class for Year-Month Dates
#'
#' A simple data type for storing year-month dates in a human readable integer
#' format, e.g.: December 2012 is stored as 201212. Supports simple arithmetics
#' (`+` and `-`) as well formatting.
#'
#' @param y year
#' @param m month (optional)
#'
#' @return `date_ym` returns an object of type `date_ym`
#' @export
#' @family simple dates
#' @seealso [format.date_ym()]
#'
#' @examples
#' date_ym(2013, 12)
#'
date_ym <- function(y, m) {
  stopifnot(is.numeric(y) || all(is.na(y)))
  stopifnot(is.numeric(m) || all(is.na(m)))
  stopifnot(all(m %in% c(1:12) | is.na(m)))

  s <- ifelse(sign(y) >= 0, 1L, -1L)
  res <- (as.integer(abs(y)) * 100L + as.integer(m)) * s

  date_xx(res, "date_ym")
}




# as_data_ym --------------------------------------------------------------

#' @param x any R object
#' @return `is_date_ym` returns `TRUE` or `FALSE` depending on whether its
#'   argument is of type `date_ym` or not.
#'
#' @export
#' @rdname date_ym
is_date_ym <- function(x){
  inherits(x, "date_ym")
}




#' @return `as_date_ym` attempts to coerce its argument to `date_ym` type
#' @export
#' @rdname date_ym
#'
#' @examples
#' as_date_ym(201612)
#'
as_date_ym <- function(x){
  UseMethod("as_date_ym")
}




#' @export
as_date_ym.date_ym <- function(x){
  x
}




#' @export
as_date_ym.default <- function(x){
  as_date_ym.Date(as.Date(x))
}




#' @export
as_date_ym.numeric <- function(x){
  stopifnot(all(x > 0 | x <= -101L))
  d <- yms_matrix_from_numeric(x)
  date_ym(y = d[, 1] * d[, 3], m = d[, 2])
}




#' @export
as_date_ym.Date <- function(x){
  y <- get_year(x)
  m <- get_month(x)
  date_ym(y = y, m = m)
}




# as.Date -----------------------------------------------------------------

#' @rdname as.Date.date_xx
#' @export
#'
as.Date.date_ym <- function(x, ...){
  make_date(
    get_year(x),
    get_month(x),
    1L
  )
}




# format ------------------------------------------------------------------

#' Format a date_ym Object
#'
#' @param x a [date_ym] object
#' @param format A scalar character, valid values are: `"iso"`, `"short"`, and
#'   `"shorter"`
#' @param ... ignored
#'
#' @return A character vector
#'
#' @export
#' @examples
#'
#' x <- date_ym(2015, 12)
#'
#' format(x, format = "iso")
#' # [1] "2015-M12"
#'
#' format(x, format = "short")
#' # [1] "2015.12"
#'
#' format(x, format = "shorter")
#' # [1] "15.12"
#'
format.date_ym <- function(
  x,
  format = "iso",
  ...
){
  switch(
    tolower(format),
    "iso"     = format_date_ym_iso(x),
    "short"   = format_date_ym_short(x),
    "shorter" = format_date_ym_shorter(x),
    stop("wrong format specified")
  )
}




#' @rdname format.date_ym
#' @export
format_date_ym_iso <- function(x){
  d <- yms_matrix_from_numeric(as_date_ym(x))
  sprintf("%s-M%02i", d[, 1] * d[, 3], d[, 2])
}




#' @rdname format.date_ym
#' @export
format_date_ym_short <- function(x){
  d <- yms_matrix_from_numeric(as_date_ym(x))
  sprintf("%s.%02i", d[, 1] * d[, 3], d[, 2])
}




#' @rdname format.date_ym
#' @export
format_date_ym_shorter <- function(x){
  d <- yms_matrix_from_numeric(as_date_ym(x))
  y <- substr_right(d[, 1], 2)  # substr so we dont want to loose leading zeroes
  y <- ifelse(d[, 3] < 0, paste0("-", y), y)
  sprintf("%s.%02i", y, d[, 2])
}




# algebra -----------------------------------------------------------------

#' @rdname date_xx_arithmetic
#' @export
`+.date_ym` <- function(x, y){
  increment(x, as.integer(y))
}




#' @rdname date_xx_arithmetic
#' @export
`-.date_ym` <- function(x, y){
  increment(x, as.integer(-y))
}




#' @rdname date_xx_arithmetic
#' @export
`*.date_ym` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic
#' @export
`/.date_ym` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic
#' @export
`^.date_ym` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic
#' @export
`%%.date_ym` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic
#' @export
`%/%.date_ym` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_ym <- function(x, y, ...){
  res <- seq.int(as.integer(x), as.integer(y))
  as_date_ym(res[(res %% 100) %in% 1:12])
}




# shortcuts ---------------------------------------------------------------

#' Directly Create Formatted Year-Month Strings
#'
#' @param x,m Two integer (vectors). `m` is optional and the interpretation of
#'   `x` will depend on whether `m` is supplied or not:
#'   * if only `x` is supplied, `x` will be passed to [as_date_ym()]
#'     (e.g. `x = 201604` means April 2016)
#'   * if `x` and `m` are supplied, `x` is interpreted as year and `m` as
#'     month.
#'
#' @inherit format.date_ym
#'
#' @family ym convenience functions
#' @seealso [format.date_ym()]
#' @export
#' @examples
#'
#' format_ym(2015, 5)
#' format_ym(201505, format = "short")
#' format_ym(201505, format = "shorter")
#'
format_ym <- function(x, m = NULL, format = "iso"){
  if (is.null(m)){
    d <- as_date_ym(x)
  } else {
    d <- date_ym(x, m)
  }

  format(d, format = format)
}




# utils -------------------------------------------------------------------

yms_matrix_from_numeric <- function(x){
  x <- unclass(x)
  matrix(
    c(abs(x) %/% 100L, m = abs(x) %% 100L, s = as.integer(sign(x))),
    ncol = 3
  )
}
