# yearqtr -----------------------------------------------------------------

#' For compat with zoo
#'
#' Internaly used constructor. If you use zoo, please use [zoo::yearqtr()]
#' instead
#'
#' @param x a vector with dates in the form 2000.0 for Q1, 2000.25 for Q2, usw
#' @noRd
#'
yearqtr <- function(x){
  assert(all((x %% 1) %in% c(0, 0.25, 0.5, 0.75)))
  structure(x, class = c("yearqtr", "numeric"))
}




#' Coerce to zoo yeartqr objects
#'
#' `as_yearqtr()` is included for interoperatility with [zoo::yearqtr()],
#' an alternative year-quarter format that is based on a decimal representation
#' as opposed to dint's integer representation of year-quarters.
#'
#' @param x any \R object
#'
#' @return a [zoo::yearqtr] vector
#' @export
#'
#' @examples
#'
#' x <- date_yq(2016, 2)
#' as_yearqtr(x)
#'
as_yearqtr <- function(x){
  UseMethod("as_yearqtr")
}




#' @rdname as_yearqtr
#' @export
as_yearqtr.date_yq <- function(x){
  yearqtr(get_year(x) + (get_quarter(x) - 1L) / 4)
}




#' @rdname as_yearqtr
as_yearqtr.yearqtr <- function(x){
  x
}




# yearmon -----------------------------------------------------------------


#' For compat with zoo
#'
#' Internaly used constructor. If you use zoo, please use [zoo::yearmon()]
#' instead
#'
#' @param x a vector with dates in the form 2000.0 for Q1, 2000.25 for Q2, usw
#' @noRd
#'
yearmon <- function(x){
  x <- as.numeric(x)
  assert(
    all(round(x %% 1, 5) %in% round(seq(0, 1, by = 1/12), 5))
  )
  structure(x, class = c("yearmon", "numeric"))
}




#' @rdname as_yearqtr
#' @export
#'
as_yearmon <- function(x){
  UseMethod("as_yearmon")
}




#' @rdname as_yearqtr
#' @export
as_yearmon.date_ym <- function(x){
  yearmon(get_year(x) + (get_month(x) - 1L) / 12)
}




#' @rdname as_yearqtr
as_yearmon.yearmon <- function(x){
  x
}



# yearweek -----------------------------------------------------------------


#' For compat with zoo
#'
#' Internaly used constructor. If you use zoo, please use [zoo::yearweek()]
#' instead
#'
#' @param x a vector with dates in the form 2000.0 for Q1, 2000.25 for Q2, usw
#' @noRd
#'
yearweek <- function(x){
  x <- as.numeric(x)
  assert(
    all(round(x %% 1, 5) %in% round(seq(0, 1, by = 1/53), 5))
  )
  structure(x, class = c("yearweek", "numeric"))
}




#' @rdname as_yearqtr
#' @export
#'
as_yearweek <- function(x){
  UseMethod("as_yearweek")
}




#' @rdname as_yearqtr
#' @export
as_yearweek.date_yw <- function(x){
  year <- get_year(x)
  last_da

  yearweek(get_year(x) + (get_month(x) - 1L) / 53)
}




#' @rdname as_yearqtr
as_yearweek.yearweek <- function(x){
  x
}



# zoo dynamic s3 mehtods --------------------------------------------------

# dynamically registered if zoo is installed
as.yearqtr.date_yq <- as_yearqtr.date_yq




as.yearmon.date_ym <- as_yearmon.date_ym
