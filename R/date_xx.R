# ctor --------------------------------------------------------------------

#' Simple Date Formats - Superclass
#'
#' Superclass for `date_xx` objects, such as [date_ym] and [date_yq].
#'
#' @param x Any r object
#' @param subclass subclass to assign
#'
#' @return a `date_xx` object
#' @export
#' @md
#'
date_xx <- function(x, subclass){
  attr(x, "class") <- union(subclass, c("date_xx", "integer"))
  x
}




#' @param y,q,m Year, Quarter, Month. Quarter and Month are optional and only
#'   one of both can be set at the same time.
#'
#' @rdname date_xx
#' @export
#' @return `make_date_xx()` is a helper that returns a valid `date_xx`
#'   object for assignment as reporting period.
make_date_xx <- function(y, q = NULL, m = NULL){
  if (!is.null(q)){
    assert_that(is.null(m))
    date_yq(y, q)
  } else if (!is.null(m)){
    date_ym(y, m)
  } else {
    date_y(y)
  }
}




# is_date_xx --------------------------------------------------------------

#' @return `is_date_xx` returns `TRUE` or `FALSE` depending on whether its
#'   argument is of type `date_xx` or not.
#'
#' @rdname date_xx
#' @export
is_date_xx <- function(x){
  inherits(x, "date_xx")
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
  lubridate::as_datetime(as.Date(x))
}
