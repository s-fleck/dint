# Group generics ----------------------------------------------------------

#' Maxima and Minima for date_xx
#'
#' @param ... `date_xx` vectors with the same subclass
#' @inheritParams base::Summary
#'
#' @return
#'   for `min()` and `max()` a scalar of the same `date_xx` subclass as it's
#'   input, for range a vector of length 2
#'
#' @export
#'
#' @examples
#' min(date_yq(2014, 1), date_yq(2014, 2))
#'
#' # raises an error
#' try(min(date_yq(2014, 1), date_ym(2014, 2)))
Summary.date_xx <- function (
  ...,
  na.rm
){
  assert(
    .Generic %in% c("min", "max", "range"),
    .Generic, "not defined for 'date_xx' Objects"
  )
  dots  <- list(...)
  class <- vapply(dots, which_date_xx, character(1))
  assert(
    all_are_identical(class),
    "All inputs must have the same <date_xx> subclass"
  )

  res <- NextMethod(.Generic)

  switch(
    class[[1]],
    date_yq = as_date_yq(res),
    date_ym = as_date_ym(res),
    date_yw = as_date_yw(res),
    date_y = as_date_y(res)
  )
}




#' Comparison Operators for date_xx
#'
#' @param e1,e2 Objects with the same `date_xx` subclass (one of them can also
#'   be integer)
#'
#' @return a `logical` scalar
#' @export
#'
#' @examples
#' date_yq(2015, 1) < date_yq(2015, 2)
#'
#' # comparison with integers is ok
#' date_yq(2015, 1) < 20152
#'
#' # but two different date_xx cannot be compared#'
#' try(date_yq(2015, 1) < date_ym(2015, 2))
#'
Ops.date_xx <- function (
  e1,
  e2
){
  assert(
    nargs() > 1L,
    sprintf("unary %s not defined for <date_xx> objects", .Generic)
  )
  assert(
    .Generic %in% c("<", ">", "==", "!=", "<=", ">="),
    sprintf("%s is not defined for <date_xx> objects", .Generic)
  )

  assert(
    (is_date_yq(e1) && is_date_yq(e2))  ||
    (is_date_ym(e1) && is_date_ym(e2))  ||
    (is_date_yw(e1) && is_date_yw(e2))  ||
    (is_date_y(e1)  && is_date_y(e2))   ||
    (!is_date_xx(e1) && is_date_xx(e2)) ||
    (!is_date_xx(e2) && is_date_xx(e1)),
    sprintf(paste(
      "Comparison of <date_xx> is only defined if both objects have the same",
      "subclass, but 'e1' is <%s> and 'e2' is <%s>"),
      which_date_xx(e1),
      which_date_xx(e2)
    )
  )

  if (is.character(e1))
    e1 <- as.Date(e1)
  if (is.character(e2))
    e2 <- as.Date(e2)

  NextMethod(.Generic)
}




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



# range -------------------------------------------------------------------



range.date_yq <- function(..., na.rm = FALSE) {
  as_date_yq(range(as.integer(c(...), na.rm = na.rm)))
}




range.date_ym <- function(..., na.rm = FALSE) {
  as_date_ym(range(as.integer(c(...), na.rm = na.rm)))
}




range.date_yw <- function(..., na.rm = FALSE) {
  as_date_yw(range(as.integer(c(...), na.rm = na.rm)))
}




range.date_y <- function(..., na.rm = FALSE) {
  as_date_y(range(as.integer(c(...), na.rm = na.rm)))
}



# round -------------------------------------------------------------------

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
ceiling.date_yq <- function(x){
  date_yq(get_year(x) + 1L, 1L)
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
floor.date_yq <- function(x){
  date_yq(get_year(x), 1L)
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
round.date_yq <- function(x){
  q <- get_quarter(x)
  ifelse_simple(q %in% 1:2, floor(x), ceiling(x))
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
