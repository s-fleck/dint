# yearqtr -----------------------------------------------------------------

#' Coerce to zoo yearqtr objects
#'
#' `as_yearqtr()` and `as_yearmon()` are included for interoperability with
#' [zoo::yearqtr()], an alternative year-quarter format that is based on a
#' decimal representation as opposed to dint's integer representation of
#' year-quarters. `as_yearweek()` follows a similar idea, but there is no
#' corresponding S3 class in \pkg{zoo}. These functions were included
#' for cases where you need a continuous representation of `date_xx` objects
#' other then [base::Date()] (for example, they are used by [scale_date_xx])
#'
#' @param x any \R object
#'
#' @return a [zoo::yearqtr], [zoo::yearmon] or `dint::yearweek` vector.
#' @export
#'
#' @examples
#' q <- date_yq(2016, 1:4)
#' as.numeric(q)
#' qzoo <- as_yearqtr(q)
#' as.numeric(qzoo)
#'
#' m <- date_ym(2016, 1:12)
#' as.numeric(m)
#' mzoo <- as_yearmon(m)
#' as.numeric(mzoo)
#'
#' w <- date_yw(2016, 1:52)
#' as.numeric(w)
#' wzoo <- as_yearweek(w)
#' as.numeric(wzoo)
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
#' @export
as_yearqtr.yearqtr <- function(x){
  x
}




#' For Compatibility With zoo
#'
#' Internaly used constructor. If you use zoo, please use [zoo::yearqtr()]
#' instead
#'
#' @param x a vector with dates in the form 2000.0 for Q1, 2000.25 for Q2, usw
#' @noRd
#'
yearqtr <- function(x){
  assert(all((x %% 1) %in% c(NA_real_, 0, 0.25, 0.5, 0.75)))
  structure(x, class = c("yearqtr", "numeric"))
}




# yearmon -----------------------------------------------------------------

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



#' @export
#' @rdname as_yearqtr
as_yearmon.yearmon <- function(x){
  x
}




#' For Compatibility With zoo
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
    all(round(x %% 1, 5) %in% c(NA_real_, round(seq(0, 1, by = 1/12), 5)))
  )
  structure(x, class = c("yearmon", "numeric"))
}




# yearweek -----------------------------------------------------------------

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
  yearweek(get_year(x) + (get_isoweek(x) - 1L) / 53)
}




#' @export
#' @rdname as_yearqtr
as_yearweek.yearweek <- function(x){
  x
}




#' For Compatibility With zoo
#'
#' Internaly used constructor. If you use zoo, please use `zoo::yearweek()`
#' instead
#'
#' @param x a vector with dates in the form 2000.0 for Q1, 2000.25 for Q2, usw
#' @noRd
yearweek <- function(x){
  x <- as.numeric(x)
  assert(
    all(round(x %% 1, 5) %in% c(NA_real_, round(seq(0, 1, by = 1/53), 5)))
  )
  structure(x, class = c("yearweek", "numeric"))
}




# zoo dynamic s3 mehtods --------------------------------------------------

# dynamically registered if zoo is installed
as.yearqtr.date_yq <- as_yearqtr.date_yq




as.yearmon.date_ym <- as_yearmon.date_ym
