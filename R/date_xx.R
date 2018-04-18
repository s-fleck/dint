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




# accessors ---------------------------------------------------------------

#' Get years component of a date_xx
#'
#' `year()` is there for consistency with [lubridate], `get_year()` is there
#' for consistency with other `get_` functions in \pkg{hammr}.
#'
#' @param x a [date_xx] object
#' @family date_xx getters
#'
#' @md
#' @export
#' @aliases year
#'
#' @examples
#'
#' \dontrun{
#' x <- date_yq(2016, 2)
#'
#' year(x)
#' get_year(x)
#' }
#'
get_year <- function(x){
  UseMethod("get_year")
}




#' @export
#' @rdname get_year
year.date_xx <- function(x){
  get_year(x)
}




#' Get quarter of a date_xx.
#'
#' `get_quarter()` returns the quarter of a [date_xx] object.
#' [lubridate::quarter()] also works, because \pkg{hammr} exports a method for
#' [lubridate::month()]. This will have more overhead than using
#' `get_quarter()`, but should generally be prefered for consistency in
#' function naming across packages.
#'
#' @inheritParams lubridate::year
#' @seealso [lubridate::quarter()]
#' @aliases quarter
#' @family yq getters
#' @export
#'
#' @examples
#'
#' x <- date_yq(2016, 2)
#'
#' quarter(x)
#' get_quarter(x)
#'
get_quarter <- function(x){
  UseMethod("get_quarter")
}




#' @export
#' @rdname get_month
month.date_xx <- function(
  x,
  label = FALSE,
  abbr = TRUE,
  locale = Sys.getlocale("LC_TIME")
){
  lubridate::month(get_month(x), label = label, abbr = abbr, locale = locale)
}




#' Get month of a date_xx
#'
#' `get_month()` returns the month of a `date_xx` object.
#' A method for [lubridate::month()] is also exported. This will have
#' slightly more overhead than using `get_month()`, but should generally be
#' prefered for consistency in function naming across packages.
#'
#' @param x a [date_xx] object.
#' @inheritParams lubridate::month
#' @rdname get_month
#' @aliases month
#' @seealso [lubridate::month()]
#' @family yq getters
#' @export
#'
#' @examples
#'
#' x <- date_yq(2016, 2)
#'
#' month(x)
#' month(x, label = TRUE)
#' get_month(x)
#'
get_month <- function(x){
  UseMethod("get_month")
}
