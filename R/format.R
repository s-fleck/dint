#' Format a date_xx
#'
#' @param x any \R object.
#' @param format A format that uses a subset of the same placeholders as
#' [base::strptime()]:
#' \tabular{rl}{
#'    \%Y \tab Year with century (the full year)\cr
#'    \%y \tab Year without century (the last two digits of the year)\cr
#'    \%m \tab Month as a decimal numbers (1-12)\cr
#'    \%B \tab Full month name\cr
#'    \%b \tab Abbreviated month name
#'  }
#'
#' @param month_names,month_abb a `character` vector of length 12: Names and
#'   abbreviations for months that will be used for the
#'   placeholders `"%b"` and `"%B"`. Defaults to the values for
#'   the current locale for compatbilioty with [base::strptime()].
#' @param ... ignored
#'
#' @return
#' @export
#'
#' @examples
#' x <- date_ym(2018, c(1L, 10L, 3L, 6L, 4L, 5L, 7L, 12L, 2L, 9L, 8L, 11L))
#' fm <- "%Y-M%m: %B,%b"
#'
#' format.date_xx(
#'   x,
#'   format = fm,
#'   month_names = month.name,  # built-in R constant for English names
#'   month_abb = month.abb
#' )
#'
format.date_xx <- function(
  x,
  format,
  month_names = format(ISOdate(2000, 1:12, 1), "%B"),
  month_abb = format(ISOdate(2000, 1:12, 1), "%b"),
  ...
){
  stopifnot(
    is_scalar_character(format),
    is.character(month_names) && identical(length(month_names), 12L),
    is.character(month_abb) && identical(length(month_abb), 12L)
  )

  if (identical(length(x), 0L))
    return(character())

  tokens <- tokenize_format(format)
  len <- length(tokens)

  res <- vector("list", length(tokens))

  year    <- get_year(x)
  month   <- get_month(x)
  quarter <- get_quarter(x)
  week    <- get_isoweek(x)

  for(i in seq_len(len)){
    if (identical(tokens[[i]], "%Y"))
      res[[i]] <- year
    else if (identical(tokens[[i]], "%y"))
      res[[i]] <- year %% 100
    else if (identical(tokens[[i]], "%q"))
      res[[i]] <- quarter
    else if (identical(tokens[[i]], "%m"))
      res[[i]] <- month
    else if (identical(tokens[[i]], "%W"))
      res[[i]] <- isoweek
    else if (identical(tokens[[i]], "%B"))
      res[[i]] <- factor(month, levels = 1:12, labels = month_names)
    else if (identical(tokens[[i]], "%b"))
      res[[i]] <- factor(month, levels = 1:12, labels = month_abb)
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
  ...
){
  format.date_xx(x, format = format, ...)
}




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
  format = "%Y-M%q",
  ...
){
  format.date_xx(x, format = format, ...)
}




#' Format a date_yw Object
#'
#' @param x a [date_yw] object
#' @param format A scalar character, valid values are: `"iso"`, `"short"`, and
#'   `"shorter"`
#' @param ... ignored
#'
#' @return A character vector
#'
#' @export
#' @examples
#'
#' x <- date_yw(2015, 12)
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
format.date_yw <- function(
  x,
  format = "%Y-W%W",
  ...
){
  format.date_xx(x, format = format, ...)
}




# utils -------------------------------------------------------------------

tokenize_format <- function(
  x
){
  pos <- unlist(gregexpr("(%Y)|(%y)|(%q)|(%m)|(%b)|(%B)|(%W)", x))
  if (identical(pos, -1L)) return(x)
  pos <- sort(unique(c(1L, pos, pos + 2L, nchar(x) + 1L)))
  res <- vector("character", length(x))
  begin <- 1L
  for(i in seq_len(length(pos) -1L)) {
    res[[i]] <- substr(x, pos[[i]], pos[[i + 1]] - 1L)
  }

  res
}



# presets -----------------------------------------------------------------

# switch(
#   tolower(format),
#   "iso"     = format_date_ym_iso(x),
#   "short"   = format_date_ym_short(x),
#   "shorter" = format_date_ym_shorter(x),
#   stop("wrong format specified")
# )
