# generics

#' Date Arithmetic Operations
#'
#' The arithmetic operations `+`, `-` as well as sequence generation with
#' `seq()` are all supported for `date_yq` and `date_ym` objects. Other binary
#' arithmetic operators are disabled (see [date_xx_arithmetic_disabled]).
#'
#' @param x a [`date_yq`] or [`date_ym`] object
#' @param y an integer
#' @param ... currently ignored
#'
#' @name date_xx_arithmetic
#' @seealso  [base::Arithmetic]
#'
#' @examples
#' q <- date_yq(2018, 1)
#' q + 5
#' q - 1
#' seq(q, q + 5)
#'
#'
#' m <- date_ym(2018, 12)
#' m + 1
#' m - 13
#' seq(m - 1, m + 1)
NULL




#' Disabled Date Arithmetic Operations
#'
#' This page lists operators that are disabled for `date_yq` and `date_ym`
#' objects.
#'
#' @inheritParams date_xx_arithmetic
#' @name date_xx_arithmetic_disabled
#' @seealso [date_xx_arithmetic], [base::Arithmetic]
NULL




#' Extract Elements of a date_xx
#'
#' Works exactly like susbetting base vectors via `[`, but preserves the
#' `date_xx` class and subclasses
#'
#' @inheritParams base::Extract
#'
#' @return a `date_xx` vector
#' @export
#'
`[.date_xx` <- function(x, i){
  r <- `[`(as.integer(x), i)
  class(r) <- class(x)
  r
}




#' @export
`[<-.date_yq` <- function(
  x,
  i,
  value
){
  assert(
    is_date_yq_integerish(value),
    "'value' is not a valid date_yq  (yyyyq)"
  )
  r <- `[<-`(as.integer(x), i, as.integer(value))
  class(r) <- class(x)
  r
}




#' @export
`[<-.date_ym` <- function(
  x,
  i,
  value
){
  assert(
    is_date_ym_integerish(value),
    "'value' is not a valid date_ym (yyyymm)"
  )
  r <- `[<-`(as.integer(x), i, as.integer(value))
  class(r) <- class(x)
  r
}




#' @export
`[<-.date_yw` <- function(
  x,
  i,
  value
){
  assert(
    is_date_yw_integerish(value),
    "'value' is not a valid date_yw integer (yyyyww)"
  )
  r <- `[<-`(as.integer(x), i, as.integer(value))
  class(r) <- class(x)
  r
}




#' @export
`[[.date_xx` <- `[.date_xx`




#' @export
`[[<-.date_yq` <- function(
  x,
  i,
  value
){
  assert(
    is_scalar_date_yq_integerish(value),
    "'value' is not a valid date_yq  (yyyyq)"
  )
  r <- `[[<-`(as.integer(x), i, as.integer(value))
  class(r) <- class(x)
  r
}




#' @export
`[[<-.date_ym` <- function(
  x,
  i,
  value
){
  assert(
    is_scalar_date_ym_integerish(value),
    "'value' is not a valid date_ym (yyyymm)"
  )
  r <- `[[<-`(as.integer(x), i, as.integer(value))
  class(r) <- class(x)
  r
}




#' @export
`[[<-.date_yw` <- function(
  x,
  i,
  value
){
  assert(
    is_scalar_date_yw_integerish(value),
    "'value' is not a valid date_yw integer (yyyyww)"
  )
  r <- `[[<-`(as.integer(x), i, as.integer(value))
  class(r) <- class(x)
  r
}
