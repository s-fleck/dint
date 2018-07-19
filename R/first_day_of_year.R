

#' Conveniently get first/last day of year from numbers
#'
#' @inheritParams format_y
#' @inherit first_day_of_year
#'
#' @family ym convenience functions
#' @seealso [first_day_of_year()]
#' @export
#' @md
#'
#' @examples
#'
#' first_day_y(2016, 1)
#' first_day_y(20161)
#'
first_day_y <- function(x, m = NULL){
  if (is.null(m)){
    d <- as_date_y(x)
  } else {
    d <- date_y(x, m)
  }

  first_day_of_year(d)
}




#' @rdname first_day_y
#' @export
last_day_y <- function(x, m = NULL){
  if (is.null(m)){
    d <- as_date_y(x)
  } else {
    d <- date_y(x, m)
  }

  last_day_of_year(d)
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
#'
#' first_day_of_year("2016-06-04")
#' last_day_of_year("2016-06-04")
#'
first_day_of_year <- function(x){
  UseMethod("first_day_of_year")
}




#' @rdname day_of_year
#' @export
first_day_of_year.default <- function(x){
  assert_lubridate()
  lubridate::floor_date(as.Date(x), "year")
}




#' @rdname day_of_year
#' @export
last_day_of_year <- function(x){
  UseMethod("last_day_of_year")
}




#' @rdname day_of_year
#' @export
last_day_of_year.default <- function(x){
  assert_lubridate()
  lubridate::ceiling_date(as.Date(x), "year") - 1L
}
