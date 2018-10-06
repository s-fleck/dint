# ctor --------------------------------------------------------------------

#' A Superclass For All dint Objects
#'
#' @description
#' Superclass for [date_yq], [date_ym], [date_yw], and [date_y].
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
  print(format(x, ...))
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




Sys.date_yq <- function() as_date_yq(Sys.Date())




Sys.date_ym <- function() as_date_ym(Sys.Date())




Sys.date_yw <- function() as_date_yw(Sys.Date())




# common generics ---------------------------------------------------------

#' Concatenate date_xx Objects
#'
#' @param ... `date_yq`, `date_ym`, `date_yw` or `date_y` vectors. All inputs
#'   must be of the same type (or its unclassed integer equivalent) or faulty
#'   output is to be expected
#'
#' @return a vector of the same `date_xx` subclass as the first element of `...`
#' @export
#'
#' @examples
#'
#' c(date_yq(2000, 1:2), date_yq(2000, 3:3))
#'
#' # raises an error
#' try(c(date_yq(2000, 1:2), date_ym(2000, 1:12)))
#'
c.date_xx <- function(...){
  dots <- list(...)
  assert(
    all(vapply(dots, is.atomic, logical(1))),
    "All inputs to c.date_xx() must be atomic vectors (i.e. no lists)"
  )
  assert(
    all_are_identical(vapply(dots, which_date_xx, character(1))),
    "All inputs to c.date_xx() must be of the same <date_xx> subclass"
  )

  res  <- unlist(dots)

  if (is_date_yq(..1))
    as_date_yq(res)
  else if (is_date_ym(..1))
    as_date_ym(res)
  else if (is_date_yw(..1))
    as_date_yw(res)
  else if (is_date_y(..1))
    as_date_y(res)
}




#' @export
unique.date_yw <- function(x, incomparables = FALSE, ...) {
  as_date_yw(unique.default(x, incomparables = incomparables, ...))
}




#' @export
unique.date_ym <- function(x, incomparables = FALSE, ...) {
  as_date_ym(unique.default(x, incomparables = incomparables, ...))
}




#' @export
unique.date_yq <- function(x, incomparables = FALSE, ...) {
  as_date_yq(unique.default(x, incomparables = incomparables, ...))
}




#' @export
unique.date_y <- function(x, incomparables = FALSE, ...) {
  as_date_y(unique.default(x, incomparables = incomparables, ...))
}




#' @export
summary.date_xx <- function(object, ...){
  summary(as.numeric(object), ...)
}




#' @export
xtfrm.date_xx <- function(x){
  as.numeric(x)
}
