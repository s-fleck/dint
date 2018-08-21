# ctor --------------------------------------------------------------------

#' A Superclass For All dint Objects
#'
#' @description
#' Superclass for `date_xx` for [date_ym], [date_yq] and [date_y].
#'
#' `make_date_xx` can be used to create such objects when it is not know if
#' month or quarter information is available.
#'
#' `is_date_xx()` checks for `date_xx` objects.
#'
#' `date_xx()` is an internally used constructor
#' that should only be used by developers aspiring to extend the dint package.
#'
#'
#' @param x Any \R object
#' @param subclass subclass to assign
#'
#' @return
#'   a `date_xx` Object, except for `is_date_xx()` which returns `TRUE` or
#'   `FALSE`
#'
#' @examples
#' make_date_xx(2017)
#' make_date_xx(2017, 4)
#' x <- make_date_xx(2017, m = 4)
#'
#' is_date_xx(x)
#'
date_xx <- function(x, subclass){
  attr(x, "class") <- union(subclass, c("date_xx", "integer"))
  x
}




#' @param y,q,m Year, quarter, month. `q` and `m` are optional and at least one
#'   of them must be `NULL`.
#'
#' @rdname date_xx
#' @export
#' @return
#'   a `date_xx` Object for `date_xx()`, `make_date_xx`
#'
make_date_xx <- function(
  y,
  q = NULL,
  m = NULL
){
  if (!is.null(q)){
    stopifnot(is.null(m))
    date_yq(y, q)
  } else if (!is.null(m)){
    date_ym(y, m)
  } else {
    date_y(y)
  }
}




# is_date_xx --------------------------------------------------------------

#' @return `is_date_xx()` returns `TRUE` or `FALSE` depending on whether its
#'   argument is of type `date_xx` or not.
#'
#' @rdname date_xx
#' @export
is_date_xx <- function(x){
  inherits(x, "date_xx")
}




# print -------------------------------------------------------------------

#' Print a date_xx Object
#'
#' @param x A [date_xx] object
#' @param ... passed on to [format.date_yq()] or [format.date_ym()]
#'
#' @return `x` (invisibly)
#' @export
#'
print.date_xx <- function(
  x,
  ...
){
  cat(format(x, ...))
  invisible(x)
}




# as.Date -----------------------------------------------------------------


#' Coerce dint Objects to Base R Date Types
#'
#' All \pkg{dint} objects can be coerced to base R Date or Datetime (`POSIXct`)
#' types. The resulting date will always default to the first possible
#' Date/Datetime in this period.
#'
#' If \pkg{lubridate} is loaded, methods for lubridate generics (such as
#' [lubridate::month()] and [lubridate::year()]) are also made available by
#' dint.
#'
#'
#' @param x any \R object
#' @param ... passed on to methods
#'
#' @return An Object of the appropriate base \R type (`Date`, `POSIXct`, or
#'   `POSIXlt`)
#'
#' @rdname as.Date.date_xx
#' @name as.Date.date_xx
#'
#' @examples
#' as.Date(date_yq(2017, 2))
#' as.POSIXlt(date_yq(2017, 2))
#'
#' # When coercing to datetime, the default timezone is UTC
#' as.POSIXct(date_yq(2017, 2))
#'




#' @rdname as.Date.date_xx
#' @inheritParams base::as.POSIXlt
#' @export
as.POSIXlt.date_xx <- function(x, tz = "", ...){
  as.POSIXlt(as.Date(x), tz = tz, ...)
}




#' @rdname as.Date.date_xx
#' @inheritParams base::as.POSIXlt
#' @export
as.POSIXct.date_xx <- function(x, tz = "", ...){
  as.POSIXct(as.Date(x, tz = tz), tz = tz, ...)

}




# arithmetics -------------------------------------------------------------

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
