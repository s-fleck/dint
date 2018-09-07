#' Test for Quarter Bounds
#'
#' `is_first_of_quarter()` and `is_last_of_quarter()` check wheter a
#' `Date` is the first or respectively the last day of an (arbitrary) quarter.
#' `is_quarter_bounds()` checks wheter two `Date` vectors mark the bounds of
#' single quarters
#'
#' @param x,first,last `Date` vectors
#'
#' @return a `logical` vector
#' @export
#'
#' @examples
#' x <- as.Date(c("2018-01-01", "2018-03-31", "2018-02-14"))
#' is_first_of_quarter(x)
#' is_last_of_quarter(x)
#' is_quarter_bounds(x[[1]], x[[2]])
#' is_quarter_bounds(x[[2]], x[[3]])
#'
is_quarter_bounds <- function(
  first,
  last
){
  is_first_of_quarter(first) &
  last_of_quarter(first) == last
}




#' @export
#' @rdname is_quarter_bounds
is_first_of_quarter <- function(
  x
){
  as_md_integer(x) %in% c(101L, 401L, 701L, 1001L)
}




#' @export
#' @rdname is_quarter_bounds
is_last_of_quarter <- function(
  x
){
  as_md_integer(x) %in% c(331L, 630L, 930L, 1231L)
}




#' @export
#' @rdname is_quarter_bounds
is_Date <- function(
  x
){
  inherits(x, "Date")
}




# utils -------------------------------------------------------------------

as_md_integer <- function(
  x
){
  assert(is_Date(x))
  r <- as.POSIXlt(x, tz = tz(x))
  (r$mon + 1L) * 100L + r$mday
}
