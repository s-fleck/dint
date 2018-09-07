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




# generics ----------------------------------------------------------------

#' Add/Subtract Year
#'
#' @param x a [date_xx] vector
#' @param y an `integer` vector of years
#'
#' @rdname y-plus
#' @export
#'
#' @examples
#' date_yq(2017, 1) %y+% 1
#' date_yq(2017, 1) %y-% 1
#' date_ym(2017, 1) %y+% 1
#' date_ym(2017, 1) %y-% 1
#'
`%y+%` <- function(x, y){
  UseMethod("%y+%")
}




#' @rdname y-plus
#' @export
`%y-%` <- function(x, y){
  UseMethod("%y-%")
}




# date_xx -----------------------------------------------------------------


#' @rdname date_xx_arithmetic
#' @export
`+.date_xx` <- function(x, y){
  increment(x, as.integer(y))
}




#' @rdname date_xx_arithmetic
#' @export
`-.date_xx` <- function(x, y){
  increment(x, as.integer(-y))
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`*.date_xx` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`/.date_xx` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`^.date_xx` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`%%.date_xx` <- function(x, y){
  stop("Operation not supported")
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`%/%.date_xx` <- function(x, y){
  stop("Operation not supported")
}




# date_y ------------------------------------------------------------------

#' @rdname y-plus
#' @export
`%y+%.date_y` <- function(x, y){
  as_date_y(as.integer(x) + y)
}




#' @rdname y-plus
#' @export
`%y-%.date_y` <- function(x, y){
  as_date_y(as.integer(x) - y)
}




# date_yq -----------------------------------------------------------------

#' @rdname y-plus
#' @export
`%y+%.date_yq` <- function(x, y){
  as_date_yq(as.integer(x) + (10L * y))
}




#' @rdname y-plus
#' @export
`%y-%.date_yq` <- function(x, y){
  as_date_yq(as.integer(x) - (10L * y))
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_yq <- function(x, y, ...){
  res <- seq.int(as.integer(x), as.integer(y))
  as_date_yq(res[(res %% 10L) %in% 1:4])
}




# date_ym -----------------------------------------------------------------

#' @rdname y-plus
#' @export
`%y+%.date_ym` <- function(x, y){
  as_date_ym(as.integer(x) + (100L * y))
}




#' @rdname y-plus
#' @export
`%y-%.date_ym` <- function(x, y){
  as_date_ym(as.integer(x) - (100L * y))
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_ym <- function(x, y, ...){
  res <- seq.int(as.integer(x), as.integer(y))
  as_date_ym(res[(res %% 100) %in% 1:12])
}




# date_yw -----------------------------------------------------------------

#' @rdname y-plus
#' @export
`%y+%.date_yw` <- function(x, y){
  as_date_yw(as.integer(x) + (100L * y))
}




#' @rdname y-plus
#' @export
`%y-%.date_yw` <- function(x, y){
  as_date_yw(as.integer(x) - (100L * y))
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_yw <- function(x, y, ...){
  res <- seq.int(as.integer(x), as.integer(y))
  as_date_yw(res[(res %% 100) %in% 1:53])
}
