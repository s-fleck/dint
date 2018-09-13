#' date_xx Arithmetic Operations
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
#'
#'
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




#' @rdname date_xx_arithmetic_disabled
#' @export
`%%.date_y` <- function(x, y){
  warning("Coercing 'date_y' to integer.")
  as.integer(x) %/% y
}




#' @rdname date_xx_arithmetic_disabled
#' @export
`%/%.date_y` <- function(x, y){
  warning("Coercing 'date_y' to integer.")
  as.integer(x) %/% y
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
