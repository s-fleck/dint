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

#' Title
#'
#' @param x A [date_xx] object
#' @param ... passed on to [format.date_yq()] or [format.date_ym()]
#'
#' @return `x` (invislby)
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


#' Coerce dint Objects to Base R Types
#'
#' All `dint` objects can be coerced to base R date Types. If `lubridate` is
#' loaded, methods for lubridate generics (such as `as_date` and `as_datetime`)
#' are also made available by dint.
#'
#' @param x any \R object
#' @param ... passed on to methods
#'
#' @return An Object of the appropriate base \R type (`Date`, `POSIXct`, or `POSIXlt`)
#'
#' @rdname as.Date.date_xx
#' @name as.Date.date_xx


#' @rdname as.Date.date_xx
#' @export
as_date.date_xx <- function(x, ...){
  assert_lubridate()
  as.Date(x, ...)
}




#' @rdname as.Date.date_xx
#' @export
as_datetime.date_xx <- function(x, ...){
  assert_lubridate()
  lubridate::as_datetime(as.Date(x), ...)
}




#' @rdname as.Date.date_xx
#' @inheritParams base::as.POSIXlt
#' @export
as.POSIXlt.date_xx <- function(x, tz = "", ...){
  as.POSIXlt(as.Date(x), tz = tz, ...)
}
