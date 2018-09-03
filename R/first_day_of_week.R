#' Get First or Last Day of a Year
#'
#' `first_day_yw()` is equivalent with `first_of_isoweek()` and only included
#' for symmetry with [first_day_ywq()] and [first_day_ywm()].
#'
#'
#' @inheritParams format_ym
#' @inherit first_of_isoweek
#'
#' @family ym convenience functions
#' @seealso [first_of_isoweek()]
#' @export
#' @rdname of_isoweek
#'
#' @examples
#' first_day_yw(2016)
#' first_day_yw(2016)
first_day_yw <- function(x, w){
  first_of_isoweek(date_yw(x, w))
}




#' @rdname of_isoweek
#' @export
last_day_yw <- function(x, w){
  last_of_isoweek(date_y(x, w))
}




#' Get first / last day of a year
#'
#' @param x Anything that can be coerced to a date with [base::as.Date()]
#'
#' @return a [Date]
#'
#' @rdname of_isoweek
#' @export
#' @examples
#' first_of_isoweek("2016-06-04")
#' last_of_isoweek("2016-06-04")
first_of_isoweek <- function(x){
  UseMethod("first_of_isoweek")
}




#' @rdname of_isoweek
#' @export
first_of_isoweek.date_yw <- function(x){
  last_of_isoweek(x) - 7L
}




#' @rdname of_isoweek
#' @export
last_of_isoweek <- function(x){
  UseMethod("last_of_isoweek")
}




#' @rdname of_isoweek
#' @export
last_of_isoweek.date_yw <- function(x){
  first_of_isoyear(get_isoyear(x)) + get_isoweek(x) * 7
}




#' @rdname of_isoweek
#' @export
first_of_isoyear <- function(x){
  UseMethod("first_of_isoyear")
}




# The first week is the week that contains the 4th of januarry
#' @rdname of_isoweek
#' @export
first_of_isoyear.date_yw <- function(x){
  first_of_isoyear(get_isoyear(x))
}




#' @rdname of_isoweek
#' @export
first_of_isoyear.integer <- function(x){
  res <- make_date(x, 1L, 4L)
  res - get_isowday(res) + 1L
}




#' @rdname of_isoweek
#' @export
first_of_isoyear.numeric <- first_of_isoyear.integer




#' @rdname of_isoweek
#' @export
last_of_isoyear <- function(x){
  UseMethod("last_of_isoyear")
}




#' @rdname of_isoweek
#' @export
last_of_isoyear.date_y <- function(x){
  res <- make_date(x + 1L, 1, 4)
  res - get_isowday(res)
}




get_isowday <- function(x){
  x <- as.POSIXlt(x)
  ifelse(x$wday == 0, 7L, x$wday)
}
