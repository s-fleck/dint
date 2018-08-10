#' Get First or Last Day of a Year
#'
#' `first_day_y()` is equivalent with `first_day_of_year()` and only included
#' for symmetry with [first_day_yq()] and [first_day_ym()].
#'
#'
#' @inheritParams format_ym
#' @inherit first_day_of_year
#'
#' @family ym convenience functions
#' @seealso [first_day_of_year()]
#' @export
#' @rdname day_of_year
#'
#' @examples
#' first_day_y(2016)
#' first_day_y(2016)
first_day_y <- function(x){
  first_day_of_year(date_y(x))
}




#' @rdname day_of_year
#' @export
last_day_y <- function(x){
  last_day_of_year(date_y(x))
}



#' Get first / last day of a year
#'
#' @param x Anything that can be coerced to a date with [base::as.Date()]
#'
#' @return a [Date]
#'
#' @rdname day_of_year
#' @md
#' @export
#' @examples
#' first_day_of_year("2016-06-04")
#' last_day_of_year("2016-06-04")
first_day_of_year <- function(x){
  UseMethod("first_day_of_year")
}




#' @rdname day_of_year
#' @export
first_day_of_year.default <- function(x){
  make_date(get_year(x), 1, 1)
}




#' @rdname day_of_year
#' @export
last_day_of_year <- function(x){
  UseMethod("last_day_of_year")
}




#' @rdname day_of_year
#' @export
last_day_of_year.default <- function(x){
  make_date(get_year(x), 12, 31)
}
