#' Get First or Last Day of a Year
#'
#' `first_day_yw()` is equivalent with `first_day_of_week()` and only included
#' for symmetry with [first_day_ywq()] and [first_day_ywm()].
#'
#'
#' @inheritParams format_ym
#' @inherit first_day_of_week
#'
#' @family ym convenience functions
#' @seealso [first_day_of_week()]
#' @export
#' @rdname day_of_week
#'
#' @examples
#' first_day_yw(2016)
#' first_day_yw(2016)
first_day_yw <- function(x, w){
  first_day_of_week(date_yw(x, w))
}




#' @rdname day_of_week
#' @export
last_day_yw <- function(x, w){
  last_day_of_week(date_y(x, w))
}



#' Get first / last day of a year
#'
#' @param x Anything that can be coerced to a date with [base::as.Date()]
#'
#' @return a [Date]
#'
#' @rdname day_of_week
#' @export
#' @examples
#' first_day_of_week("2016-06-04")
#' last_day_of_week("2016-06-04")
first_day_of_week <- function(x){
  UseMethod("first_day_of_week")
}




#' @rdname day_of_week
#' @export
first_day_of_week.default <- function(x){
  first_day_of_year(x) + (get_week(x) * 7)
}




#' @rdname day_of_week
#' @export
last_day_of_week <- function(x){
  UseMethod("last_day_of_week")
}




#' @rdname day_of_week
#' @export
last_day_of_week.default <- function(x){
  make_date(get_year(x), 12, 31)
}
