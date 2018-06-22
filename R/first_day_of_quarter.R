

#' Conveniently get first/last day of quarter from numbers
#'
#' @inheritParams format_yq
#' @inherit first_day_of_quarter
#'
#' @family yq convenience functions
#' @seealso [first_day_of_quarter()]
#' @export
#' @md
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

  first_day_of_quarter(d)
}




#' @rdname first_day_yq
#' @export
last_day_yq <- function(x, q = NULL){
  if (is.null(q)){
    d <- as_date_yq(x)
  } else {
    d <- date_yq(x, q)
  }

  last_day_of_quarter(d)
}



#' Get first / last day of a quarter
#'
#' @param x Anything that can be coerced to a date with [base::as.Date()]
#'
#' @return a [Date]
#'
#' @rdname day_of_quarter
#' @md
#' @export
#' @examples
#'
#' first_day_of_quarter("2016-06-04")
#' last_day_of_quarter("2016-06-04")
#'
first_day_of_quarter <- function(x){
  UseMethod("first_day_of_quarter")
}




#' @rdname day_of_quarter
#' @export
first_day_of_quarter.default <- function(x){
  assert_lubridate()
  lubridate::floor_date(as.Date(x), "quarter")
}




#' @rdname day_of_quarter
#' @export
last_day_of_quarter <- function(x){
  UseMethod("last_day_of_quarter")
}




#' @rdname day_of_quarter
#' @export
last_day_of_quarter.default <- function(x){
  assert_lubridate()
  lubridate::ceiling_date(as.Date(x), "quarter") - 1L
}
