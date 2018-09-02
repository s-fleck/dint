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
  format = "%Y-Q%q",
  preset = NULL,
  ...
){
  format_date_xx(x, format = format)
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

tokenize_format <- function(
  x
){
  pos <- unlist(gregexpr("(%Y)|(%y)|(%q)", x))
  if (identical(pos, -1L)) return(x)
  pos <- sort(unique(c(1L, pos, pos + 2L, nchar(x) + 1L)))
  res <- vector("character", length(x))
  begin <- 1L
  for(i in seq_len(length(pos) -1L)) {
    res[[i]] <- substr(x, pos[[i]], pos[[i + 1]] - 1L)
  }

  res
}




format_date_xx <- function(
  x,
  format
){
  assert(
    is_scalar_character(format),
    "'format' must be a character vector of length 1"
  )

  if (identical(length(x), 0L))
    return(character())

  tokens <- tokenize_format(format)
  len <- length(tokens)

  res <- vector("list", length(tokens))

  year    <- get_year(x)
  month   <- get_month(x)
  quarter <- get_quarter(x)

  for(i in seq_len(len)){
    if (identical(tokens[[i]], "%Y"))
      res[[i]] <- year
    else if (identical(tokens[[i]], "%y"))
      res[[i]] <- year %% 100
    else if (identical(tokens[[i]], "%q"))
      res[[i]] <- quarter
    else if (identical(tokens[[i]], "%m"))
      res[[i]] <- month
    else
      res[[i]] <- tokens[[i]]
  }

  res <- do.call(paste0, res)

  if (identical(length(res), length(x)))
    return(res)
  else if (identical(length(res), 1L))
    return(rep(res, length(x)))
  else
    stop("Something went wrong")

}
