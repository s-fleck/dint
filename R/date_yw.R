# ctor --------------------------------------------------------------------

#' A Simple S3-Class for Year-Week Dates
#'
#' A simple data type for storing year-week dates in a human readable integer
#' format, e.g.: December 2012 is stored as 201212. Supports simple arithmetics
#' (`+` and `-`) as well formatting.
#'
#' @param y year
#' @param m week (optional)
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
  y <- get_year(x)
  m <- get_week(x)
  date_yw(y = y, w = m)
}




# as.Date -----------------------------------------------------------------

#' @rdname as.Date.date_xx
#' @export
#'
as.Date.date_yw <- function(x, ...){
  make_date(
    get_year(x),
    get_week(x),
    1L
  )
}




# format ------------------------------------------------------------------

#' Format a date_yw Object
#'
#' @param x a [date_yw] object
#' @param format A scalar character, valid values are: `"iso"`, `"short"`, and
#'   `"shorter"`
#' @param ... ignored
#'
#' @return A character vector
#'
#' @export
#' @examples
#'
#' x <- date_yw(2015, 12)
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
format.date_yw <- function(
  x,
  format = "iso",
  ...
){
  switch(
    tolower(format),
    "iso"     = format_date_yw_iso(x),
    "short"   = format_date_yw_short(x),
    "shorter" = format_date_yw_shorter(x),
    stop("wrong format specified")
  )
}




#' @rdname format.date_yw
#' @export
format_date_yw_iso <- function(x){
  d <- yws_matrix_from_numeric(as_date_yw(x))
  sprintf("%s-W%02i", d[, 1] * d[, 3], d[, 2])
}




#' @rdname format.date_yw
#' @export
format_date_yw_short <- function(x){
  d <- yws_matrix_from_numeric(as_date_yw(x))
  sprintf("%s.%02i", d[, 1] * d[, 3], d[, 2])
}




#' @rdname format.date_yw
#' @export
format_date_yw_shorter <- function(x){
  d <- yws_matrix_from_numeric(as_date_yw(x))
  y <- substr_right(d[, 1], 2)  # substr so we dont want to loose leading zeroes
  y <- ifelse(d[, 3] < 0, paste0("-", y), y)
  sprintf("%s.%02i", y, d[, 2])
}




# algebra -----------------------------------------------------------------

#' @rdname date_xx_arithmetic
#' @export
`+.date_yw` <- function(x, y){
  increment(x, as.integer(y))
}




#' @rdname date_xx_arithmetic
#' @export
`-.date_yw` <- function(x, y){
  increment(x, as.integer(-y))
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`*.date_yw` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`/.date_yw` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`^.date_yw` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`%%.date_yw` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`%/%.date_yw` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_yw <- function(x, y, ...){
  res <- seq.int(as.integer(x), as.integer(y))
  as_date_yw(res[(res %% 100) %in% 1:53])
}




# special algebra ---------------------------------------------------------

#' @rdname y-plus
#' @export
`%y+%.date_yw` <- function(x, y){
  as_date_yw(as.integer(x) + (100 * y))
}




#' @rdname y-plus
#' @export
`%y-%.date_yw` <- function(x, y){
  as_date_yw(as.integer(x) - (100 * y))
}




# shortcuts ---------------------------------------------------------------

#' Directly Create Formatted Year-Week Strings
#'
#' @param x,m Two integer (vectors). `m` is optional and the interpretation of
#'   `x` will depend on whether `m` is supplied or not:
#'   * if only `x` is supplied, `x` will be passed to [as_date_yw()]
#'     (e.g. `x = 201604` means April 2016)
#'   * if `x` and `m` are supplied, `x` is interpreted as year and `m` as
#'     week.
#'
#' @inherit format.date_yw
#'
#' @family yw convenience functions
#' @seealso [format.date_yw()]
#' @export
#' @examples
#'
#' format_yw(2015, 5)
#' format_yw(201505, format = "short")
#' format_yw(201505, format = "shorter")
#'
format_yw <- function(x, w = NULL, format = "iso"){
  if (is.null(m)){
    d <- as_date_yw(x)
  } else {
    d <- date_yw(x, m)
  }

  format(d, format = format)
}




# utils -------------------------------------------------------------------

yws_matrix_from_numeric <- yms_matrix_from_numeric
