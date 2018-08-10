# ctor --------------------------------------------------------------------

#' A Simple S3-Class for Year-Quarter Dates
#'
#' A simple data type for storing year-quarter dates in a human readable integer
#' format, e.g.: 3.Qurter of 2012 is stored as 20123. Supports simple arithmethics
#' (`+` and `-`) as well formatting.
#'
#' @param y year
#' @param q quarter (optional)
#'
#' @return `date_yq` returns an object of type `date_yq`
#' @export
#' @family simple dates
#' @seealso [format.date_ym()]
#' @md
#'
#' @examples
#' date_yq(2013, 3)
#'
date_yq <- function(y, q) {
  stopifnot(is.numeric(y) || all(is.na(y)))
  stopifnot(is.numeric(q) || all(is.na(q)))
  stopifnot(all(q %in% c(1:4) | is.na(q)))

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
#' @md
#' @rdname date_yq
is_date_yq <- function(x){
  inherits(x, "date_yq")
}




#' @md
#' @return `as_date_yq` attempts to coerce its argument to `date_yq` type
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




# format ------------------------------------------------------------------

#' Format a date_yq Object
#'
#' @param x a [date_yq] object
#' @param format A scalar character, valid values are: `"iso"`, `"short"`, and
#'   `"shorter"`
#' @param ... ignored
#'
#' @return A character vector
#'
#' @md
#' @export
#' @examples
#'
#' x <- date_yq(2015, 3)
#'
#' format(x, format = "iso")
#' # [1] "2015-Q3"
#'
#' format(x, format = "short")
#' # [1] "2015.3"
#'
#' format(x, format = "shorter")
#' # [1] "15.3"
#'
format.date_yq <- function(
  x,
  format = "iso",
  ...
){
  switch(
    tolower(format),
    "iso"     = format_date_yq_iso(x),
    "short"   = format_date_yq_short(x),
    "shorter" = format_date_yq_shorter(x),
    stop("wrong format specified")
  )
}




#' @rdname format.date_yq
#' @export
#'
format_date_yq_iso <- function(x){
  d <- yqs_matrix_from_numeric(as_date_yq(x))
  sprintf("%s-Q%s", d[, 1] * d[, 3], d[, 2])
}




#' @rdname format.date_yq
#' @export
#'
format_date_yq_short <- function(x){
  d <- yqs_matrix_from_numeric(as_date_yq(x))
  sprintf("%s.%s", d[, 1] * d[, 3], d[, 2])
}




#' @rdname format.date_yq
#' @export
#'
format_date_yq_shorter <- function(x){
  d <- yqs_matrix_from_numeric(as_date_yq(x))
  y <- substr_right(d[, 1], 2)  # substr so we dont want to loose leading zeroes
  y <- ifelse(d[, 3] < 0, paste0("-", y), y)
  sprintf("%s.%s", y, d[, 2])
}




# algebra -----------------------------------------------------------------

#' Date Arithmetic Operations
#'
#' Currently only `+` and `-` are supported, all other basic arithmethic
#' operations are disabled for date_yq objects.
#'
#' @param x a [`date_yq`] or [`date_ym`] object
#' @param y an integer
#' @param ... currently ingored.
#'
#' @rdname date_xx_arithmetic
#' @aliases date_yq_arithmetic
#' @seealso [base::Arithmetic]
#' @export
`+.date_yq` <- function(x, y){
  increment(x, as.integer(y))
}




#' @rdname date_yq_arithmetic
#' @export
`-.date_yq` <- function(x, y){
  increment(x, as.integer(-y))
}




#' @rdname date_yq_arithmetic
#' @export
`*.date_yq` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_yq_arithmetic
#' @export
`/.date_yq` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_yq_arithmetic
#' @export
`^.date_yq` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_yq_arithmetic
#' @export
`%%.date_yq` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_yq_arithmetic
#' @export
`%/%.date_yq` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_yq_arithmetic
#' @export
seq.date_yq <- function(x, y, ...){
  res <- seq.int(as.integer(x), as.integer(y))
  as_date_yq(res[(res %% 10) %in% 1:4])
}




# shortcuts ---------------------------------------------------------------

#' Direclty Create Formatted Year-Quarter Strings
#'
#' @param x,q Two integer (vectors). `q` is optional and the interpretation of
#'   `x` will depend on whether `q` is supplied or not:
#'   * if only `x` is supplied, `x` will be passed to [as_date_yq()]
#'     (e.g. `x = 20161` means first quarter of 2016)
#'   * if `x` and `q` are supplied, `x` is interpreted as year and `q` as
#'     quarter.
#'
#' @inherit format.date_yq
#'
#' @md
#' @family yq convenience functions
#' @seealso [format.date_yq()]
#' @export
#' @examples
#'
#' format_yq(2015, 1)
#' format_yq(20151, format = "short")
#' format_yq(20151, format = "shorter")
#'
format_yq <- function(x, q = NULL, format = "iso"){
  if (is.null(q)){
    d <- as_date_yq(x)
  } else {
    d <- date_yq(x, q)
  }

  format(d, format = format)
}





# utils -------------------------------------------------------------------

yqs_matrix_from_numeric <- function(x){
  x <- unclass(x)
  matrix(
    c(abs(x) %/% 10, q = abs(x) %% 10, s = as.integer(sign(x))),
    ncol = 3
  )
}
