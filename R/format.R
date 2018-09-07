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
#' @return a `character` vector
#' @export
#'
#' @examples
#' x <- date_ym(2018, c(1L, 10L, 3L, 6L, 4L, 5L, 7L, 12L, 2L, 9L, 8L, 11L))
#' fm <- "%Y-M%m: %B,%b"
#'
#' format(
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
  yr      <- sign(year) * (abs(year) %% 100L)
  month   <- get_month(x)
  quarter <- get_quarter(x)

  for(i in seq_len(len)){
    if (identical(tokens[[i]], "%Y"))
      res[[i]] <- year
    else if (identical(tokens[[i]], "%y"))
      res[[i]] <- yr
    else if (identical(tokens[[i]], "%q"))
      res[[i]] <- quarter
    else if (identical(tokens[[i]], "%m"))
      res[[i]] <- pad_zero_left(month)
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




#' @rdname format.date_xx
#' @export
format.date_y <- function(
  x,
  format = "%Y",
  ...
){
 stopifnot(
    is_scalar_character(format)
  )

  if (identical(length(x), 0L))
    return(character())

  # init
  tokens <- tokenize_format(format)
  len <- length(tokens)
  res <- vector("list", length(tokens))

  year    <- unclass(x)
  yr      <- sign(year) * (abs(year) %% 100L)

  for(i in seq_len(len)){
    if (identical(tokens[[i]], "%Y"))
      res[[i]] <- year
    else if (identical(tokens[[i]], "%y"))
      res[[i]] <- yr
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




#' @rdname format.date_xx
#' @export
format.date_yq <- function(
  x,
  format = "%Y-Q%q",
  ...
){
  format.date_xx(x, format = format, ...)
}




#' @rdname format.date_xx
#' @export
format.date_ym <- function(
  x,
  format = "%Y-M%q",
  ...
){
  format.date_xx(x, format = format, ...)
}




#' @rdname format.date_xx
#' @export
format.date_yw <- function(
  x,
  format = "%Y-W%W",
  ...
){
 stopifnot(
    is_scalar_character(format)
  )

  if (identical(length(x), 0L))
    return(character())

  tokens <- tokenize_format(format)
  len <- length(tokens)

  res <- vector("list", length(tokens))

  year    <- get_isoyear(x)
  week    <- get_isoweek(x)

  for(i in seq_len(len)){
    if (identical(tokens[[i]], "%Y"))
      res[[i]] <- year
    else if (identical(tokens[[i]], "%y"))
      res[[i]] <- year %% 100
    else if (identical(tokens[[i]], "%W"))
      res[[i]] <- pad_zero_left(week)
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




pad_zero_left <- function(x){
  ifelse(nchar(x) == 1, paste0("0", x), x)
}


# presets -----------------------------------------------------------------

#' Format shortcuts
#'
#' @param x anything that can be coerced to Date
#' @name format_date_xx
#'
NULL


#' @rdname format_date_xx
#' @export
format_date_yq_iso     <- function(x)  format(as_date_yq(x))

#' @rdname format_date_xx
#' @export
format_date_yq_short   <- function(x)  format(as_date_yq(x), "%Y.%q")

#' @rdname format_date_xx
#' @export
format_date_yq_shorter <- function(x)  format(as_date_yq(x), "%y.%q")

#' @rdname format_date_xx
#' @export
format_date_ym_iso     <- function(x)  format(as_date_yq(x))

#' @rdname format_date_xx
#' @export
format_date_ym_short   <- function(x)  format(as_date_yq(x), "%Y.%m")

#' @rdname format_date_xx
#' @export
format_date_ym_shorter <- function(x)  format(as_date_yq(x), "%y.%m")
