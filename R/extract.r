#' Extract or Replace Elements of a date_xx
#'
#' Works exactly like susbetting base vectors via `[`, but preserves the
#' `date_xx` class and subclasses. The replacement functions `[<-` and `[[<-`
#' conduct additional checks before assignment to prevent the generation of
#' degenerate date_xx vectors (see examples).
#'
#' @inheritParams base::Extract
#' @param value A vector of the same class as `x` or a vector of integers that
#'   correspond to the internal representation `date_yq/date_ym/date_yw` objects
#'   (see examples)
#'
#' @rdname extract_date_xx
#' @return a `date_xx` vector
#' @seealso [base::Extract]
#' @export
#'
#' @examples
#' x <- date_yq(2016, 1:4)
#'
#' x[[2]]
#' x[1] <- date_yq(2016, 3)
#' x[2] <- 20164  # 2016, 4th quarter
#' x[1:2]
#'
#' # Trying to assign illegal values for the respective date_xx type raises an error
#' try(x[2] <- 20165)
#'
#' x <- date_ym(2016, 1:3)
#' x[1] <- 201610  # October 2016
#'
#' x <- date_yw(2016, 50:52)
#' x[1] <- 201649  # 2016, week 52
#'
#'
`[.date_xx` <- function(x, i){
  structure(`[`(as.integer(x), i), class = class(x))
}




#' @rdname extract_date_xx
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
  structure(
    `[<-`(as.integer(x), i, as.integer(value)),
    class = class(x)
  )
}




#' @rdname extract_date_xx
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
  structure(
    `[<-`(as.integer(x), i, as.integer(value)),
    class = class(x)
  )
}




#' @rdname extract_date_xx
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
  structure(
    `[<-`(as.integer(x), i, as.integer(value)),
    class = class(x)
  )
}




#' @rdname extract_date_xx
#' @export
`[[.date_xx` <- function(x, i){
  structure(`[[`(as.integer(x), i), class = class(x))
}




#' @rdname extract_date_xx
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
  structure(
    `[[<-`(as.integer(x), i, as.integer(value)),
    class = class(x)
  )
}




#' @rdname extract_date_xx
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
  structure(
    `[[<-`(as.integer(x), i, as.integer(value)),
    class = class(x)
  )
}




#' @rdname extract_date_xx
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
  structure(
    `[[<-`(as.integer(x), i, as.integer(value)),
    class = class(x)
  )
}
