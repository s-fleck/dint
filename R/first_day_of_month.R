#' Get First or Last Day of Month From Year and Month
#'
#' @inheritParams format_ym
#' @inherit first_day_of_month
#'
#' @family ym convenience functions
#' @seealso [first_day_of_month()]
#' @export
#' @md
#'
#' @examples
#'
#' first_day_ym(2016, 1)
#' first_day_ym(201601)
#'
first_day_ym <- function(
  x,
  m = NULL
){
  if (is.null(m)){
    d <- as_date_ym(x)
  } else {
    d <- date_ym(x, m)
  }

  first_day_of_month(d)
}




#' @rdname first_day_ym
#' @export
last_day_ym <- function(
  x,
  m = NULL
){
  if (is.null(m)){
    d <- as_date_ym(x)
  } else {
    d <- date_ym(x, m)
  }

  last_day_of_month(d)
}



#' Get first / last day of a month
#'
#' @param x Anything that can be coerced to a date with [base::as.Date()]
#'
#' @return a [Date]
#'
#' @rdname day_of_month
#' @md
#' @export
#' @examples
#'
#' first_day_of_month("2016-06-04")
#' last_day_of_month("2016-06-04")
#'
first_day_of_month <- function(x){
  UseMethod("first_day_of_month")
}




#' @rdname day_of_month
#' @export
first_day_of_month.default <- function(x){
  make_date(get_year(x), get_month(x), 1)
}




#' @rdname day_of_month
#' @export
last_day_of_month <- function(x){
  UseMethod("last_day_of_month")
}




#' @rdname day_of_month
#' @export
last_day_of_month.default <- function(x){
  month <- get_month(x)
  assert(all(month %in% 1:12))

  ifelse_simple(
    month < 12,
    make_date(get_year(x), month + 1, 1) - 1,
    make_date(get_year(x), 12, 31)
  )
}
