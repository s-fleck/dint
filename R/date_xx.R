# ctor --------------------------------------------------------------------

#' Simple Date Formats - Superclass
#'
#' Superclass for `date_xx` for [date_ym], [date_yq] and [date_y].
#' `make_date_xx` can be used to create such objects when it is not know if
#' month or quarter information is available. `is_date_xx()`
#' checks for `date_xx` objects. `date_xx()` is an internally used constructor
#' that should only be used by developers aspiring to extend the dint package.
#'
#'
#' @param x Any \R object
#' @param subclass subclass to assign
#'
#' @return `date_xx()` adds the `date_xx` subclass to any \R Object without
#'   additional checks.
#'
#' @examples
#'
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




#' @param y,q,m Year, quarter, month. Quarter and month are optional and only
#'   one of both can be set at the same time.
#'
#' @rdname date_xx
#' @export
#' @return `make_date_xx()` is constructs a `date_xx` Object from a year and
#'   either a month or quarter.
#'
make_date_xx <- function(y, q = NULL, m = NULL){
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
print.date_xx <- function(x, ...){
  cat(format(x, ...))
  invisible(x)
}




# as.Date -----------------------------------------------------------------

#' @rdname as.Date.date_ym
#' @export
as_date.date_xx <- function(x, ...){
  as.Date(x)
}




#' @rdname as.Date.date_ym
#' @export
as_datetime.date_xx <- function(x, ...){
  assert_lubridate()
  lubridate::as_datetime(as.Date(x))
}
