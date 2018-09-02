format.date_xx <- function(
  x,
  format = NULL
){
  year    <- get_year(x)
  quarter <- get_quarter(x)

  gsub(format)


}


# date_yq ------------------------------------------------------------------

#' Format a date_yq Object
#'
#' @param x a [date_yq] object
#' @param format A scalar character, valid values are: `"iso"`, `"short"`, and
#'   `"shorter"`
#' @param ... ignored
#'
#' @return A character vector
#'
#' @export
#' @examples
#'
#' x <- date_yq(2015, 3)
#'
#' format(x, format = "iso")
#' # [1] "2015-Q3"
#'
#' format(x, format = "short")
#' # [1] "2015.3"
#'
#' format(x, format = "shorter")
#' # [1] "15.3"
#'
format.date_yq <- function(
  x,
  format,
  preset = NULL,
  ...
){
  switch(
    tolower(format),
    "iso"     = format_date_yq_iso(x),
    "short"   = format_date_yq_short(x),
    "shorter" = format_date_yq_shorter(x),
    stop("wrong format specified")
  )
}




#' @rdname format.date_yq
#' @export
#'
format_date_yq_iso <- function(x){
  d <- yqs_matrix_from_numeric(as_date_yq(x))
  sprintf("%s-Q%s", d[, 1] * d[, 3], d[, 2])
}




#' @rdname format.date_yq
#' @export
#'
format_date_yq_short <- function(x){
  d <- yqs_matrix_from_numeric(as_date_yq(x))
  sprintf("%s.%s", d[, 1] * d[, 3], d[, 2])
}




#' @rdname format.date_yq
#' @export
#'
format_date_yq_shorter <- function(x){
  d <- yqs_matrix_from_numeric(as_date_yq(x))
  y <- substr_right(d[, 1], 2)  # substr so we dont want to loose leading zeroes
  y <- ifelse(d[, 3] < 0, paste0("-", y), y)
  sprintf("%s.%s", y, d[, 2])
}


# date_ym ------------------------------------------------------------------

#' Format a date_ym Object
#'
#' @param x a [date_ym] object
#' @param format A scalar character, valid values are: `"iso"`, `"short"`, and
#'   `"shorter"`
#' @param ... ignored
#'
#' @return A character vector
#'
#' @export
#' @examples
#'
#' x <- date_ym(2015, 12)
#'
#' format(x, format = "iso")
#' # [1] "2015-M12"
#'
#' format(x, format = "short")
#' # [1] "2015.12"
#'
#' format(x, format = "shorter")
#' # [1] "15.12"
#'
format.date_ym <- function(
  x,
  format = "iso",
  ...
){
  switch(
    tolower(format),
    "iso"     = format_date_ym_iso(x),
    "short"   = format_date_ym_short(x),
    "shorter" = format_date_ym_shorter(x),
    stop("wrong format specified")
  )
}




#' @rdname format.date_ym
#' @export
format_date_ym_iso <- function(x){
  d <- yms_matrix_from_numeric(as_date_ym(x))
  sprintf("%s-M%02i", d[, 1] * d[, 3], d[, 2])
}




#' @rdname format.date_ym
#' @export
format_date_ym_short <- function(x){
  d <- yms_matrix_from_numeric(as_date_ym(x))
  sprintf("%s.%02i", d[, 1] * d[, 3], d[, 2])
}




#' @rdname format.date_ym
#' @export
format_date_ym_shorter <- function(x){
  d <- yms_matrix_from_numeric(as_date_ym(x))
  y <- substr_right(d[, 1], 2)  # substr so we dont want to loose leading zeroes
  y <- ifelse(d[, 3] < 0, paste0("-", y), y)
  sprintf("%s.%02i", y, d[, 2])
}




# utils -------------------------------------------------------------------

format_date_xx <- function(
  x,
  format
){


}
