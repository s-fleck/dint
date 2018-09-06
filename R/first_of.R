# year --------------------------------------------------------------------





#' Get first / last day of a year
#'
#' @param x Anything that can be coerced to a date with [base::as.Date()]
#'
#' @return a [Date]
#'
#' @rdname day_of_year
#' @export
#' @examples
#' first_of_year("2016-06-04")
#' last_of_year("2016-06-04")
first_of_year <- function(x){
  UseMethod("first_of_year")
}




#' @rdname day_of_year
#' @export
first_of_year.integer <- function(x){
  make_date(x, 1, 1)
}




#' @rdname day_of_year
#' @export
first_of_year.default <- function(x){
  first_of_year(get_year(x))
}




#' @rdname day_of_year
#' @export
first_of_year.numeric <- function(x){
  first_of_year(as.integer(x))
}




#' @rdname day_of_year
#' @export
last_of_year <- function(x){
  UseMethod("last_of_year")
}




#' @rdname day_of_year
#' @export
last_of_year.integer <- function(x){
  make_date(x, 12, 31)
}




#' @rdname day_of_year
#' @export
last_of_year.default <- function(x){
  last_of_year(get_year(x))
}




#' @rdname day_of_year
#' @export
last_of_year.numeric <- function(x){
  last_day_of_year(as.integer(x))
}




# quarter -----------------------------------------------------------------

#' Get first / last day of a quarter
#'
#' @param x Anything that can be coerced to a date with [base::as.Date()]
#'
#' @return a [Date]
#'
#' @rdname day_of_quarter
#' @export
#' @examples
#'
#' first_of_quarter("2016-06-04")
#' last_of_quarter("2016-06-04")
#'
first_of_quarter <- function(x){
  UseMethod("first_of_quarter")
}




#' @rdname day_of_quarter
#' @export
first_of_quarter.default <- function(x){
  make_date(
    get_year(x),
    c(1, 4, 7, 10)[quarter_from_month(get_month(x))],
    1
  )
}




#' @rdname day_of_quarter
#' @export
last_of_quarter <- function(x){
  UseMethod("last_of_quarter")
}




#' @rdname day_of_quarter
#' @export
last_of_quarter.default <- function(x){
  assert_lubridate()
  lubridate::ceiling_date(as.Date(x), "quarter") - 1L
}




# month -------------------------------------------------------------------






#' Get first / last day of a month
#'
#' @param x Anything that can be coerced to a date with [base::as.Date()]
#'
#' @return a [Date]
#'
#' @rdname day_of_month
#' @export
#' @examples
#'
#' first_of_month("2016-06-04")
#' last_of_month("2016-06-04")
#'
first_of_month <- function(x){
  UseMethod("first_of_month")
}




#' @rdname day_of_month
#' @export
first_of_month.default <- function(x){
  make_date(get_year(x), get_month(x), 1)
}




#' @rdname day_of_month
#' @export
last_of_month <- function(x){
  UseMethod("last_of_month")
}




#' @rdname day_of_month
#' @export
last_of_month.default <- function(x){
  month <- get_month(x)
  assert(all(month %in% 1:12))

  ifelse_simple(
    month < 12,
    make_date(get_year(x), month + 1, 1) - 1,
    make_date(get_year(x), 12, 31)
  )
}




# isoweek -----------------------------------------------------------------




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




# isoyear -----------------------------------------------------------------



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




# shorthands --------------------------------------------------------------

#' Get First or Last Day of a Year
#'
#' `first_day_y()` is equivalent with `first_of_year()` and only included
#'
#'
#' @inheritParams format_ym
#' @inherit first_of_year
#'
#' @family ym convenience functions
#' @seealso [first_of_year()]
#' @export
#' @rdname day_of_year
#'
#' @examples
#' first_day_y(2016)
#' first_day_y(2016)
first_day_y <- function(x){
  first_of_year(date_y(x))
}




#' @rdname day_of_year
#' @export
last_day_y <- function(x){
  last_of_year(date_y(x))
}




#' Get First or Last Day of Quarter From Year and Quarter
#'
#' @inheritParams format_yq
#' @inherit first_of_quarter
#'
#' @family yq convenience functions
#' @seealso [first_of_quarter()]
#' @export
#'
#' @examples
#'
#' first_day_yq(2016, 1)
#' first_day_yq(20161)
#'
first_day_yq <- function(x, q = NULL){
  if (is.null(q)){
    d <- as_date_yq(x)
  } else {
    d <- date_yq(x, q)
  }

  first_of_quarter(d)
}




#' @rdname first_day_yq
#' @export
last_day_yq <- function(x, q = NULL){
  if (is.null(q)){
    d <- as_date_yq(x)
  } else {
    d <- date_yq(x, q)
  }

  last_of_quarter(d)
}




#' Get First or Last Day of Month From Year and Month
#'
#' @inheritParams format_ym
#' @inherit first_of_month
#'
#' @family ym convenience functions
#' @seealso [first_of_month()]
#' @export
#' @rdname day_of_month
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

  first_of_month(d)
}




#' @rdname day_of_month
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

  last_of_month(d)
}




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




# utils -------------------------------------------------------------------

quarter_from_month <- function(x){
  c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)[x]
}




get_isowday <- function(x){
  x <- as.POSIXlt(x)
  ifelse(x$wday == 0, 7L, x$wday)
}
