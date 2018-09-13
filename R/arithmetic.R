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
