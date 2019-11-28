#' Format a date_xx
#'
#' @param x any \R object.
#' @param format A format that uses a subset of the same placeholders as
#' [base::strptime()]:
#' \tabular{rl}{
#'    `%Y` \tab Year with century (the full year)\cr
#'    `%y` \tab Year without century (the last two digits of the year)\cr
#'    `%m` \tab Month as a decimal numbers (01-12)\cr
#'    `%B` \tab Full month name\cr
#'    `%b` \tab Abbreviated month name\cr
#'    `%V` \tab Week of the year as decimal number (01-53) as defined in
#'      [ISO8601](https://en.wikipedia.org/wiki/ISO_week_date)
#'  }
#'  Not all placeholders are supported for all `date_xx` subclasses.
#'  Literal `"%"` can be escaped with `"%%"` (as in [base::sprintf()]).
#'
#' @param month_names,month_abb a `character` vector of length 12: Names and
#'   abbreviations for months that will be used for the
#'   placeholders `"%b"` and `"%B"`. Defaults to the values for
#'   the current locale for compatibility with [base::strptime()].
#' @param ... ignored
#'
#'
#' @section Formatting shorthands:
#'
#' Format shorthand functions in the form of `format_y*_[preset]()` directly
#' apply formatting presets to anything  that can be coerced to a `date_xx`.
#' This is notably handy as they can be used as a labeling function for
#' **ggplot2**  axes (see `vignette("dint")`)
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
#' @name format_date_xx
NULL




#' @rdname format_date_xx
#' @export
format.date_y <- function(
  x,
  format = "%Y",
  ...
){
  format_date_xx(
    x,
    format = format,
    valid_tokens = paste0("%", c("Y", "y", "%"))
  )
}




#' @rdname format_date_xx
#' @export
format.date_yq <- function(
  x,
  format = "%Y-Q%q",
  month_names = format(ISOdate(2000, 1:12, 1), "%B"),
  month_abb = format(ISOdate(2000, 1:12, 1), "%b"),
  ...
){
  format_date_xx(
    x,
    format = format,
    valid_tokens = paste0("%", c("Y", "y", "b", "B", "m", "q", "%")),
    month_names = month_names,
    month_abb = month_abb
  )
}




#' @rdname format_date_xx
#' @export
format.date_ym <- function(
  x,
  format = "%Y-M%m",
  month_names = format(ISOdate(2000, 1:12, 1), "%B"),
  month_abb = format(ISOdate(2000, 1:12, 1), "%b"),
  ...
){
  format_date_xx(
    x,
    format = format,
    valid_tokens = paste0("%", c("Y", "y", "b", "B", "m", "q", "%")),
    month_names = month_names,
    month_abb = month_abb
  )
}




#' @rdname format_date_xx
#' @export
format.date_yw <- function(
  x,
  format = "%Y-W%V",
  ...
){
  format_date_xx(
    x,
    format = format,
    valid_tokens = paste0("%", c("Y", "y", "V", "W", "%"))
  )
}




format_date_xx <- function(
  x,
  format,
  valid_tokens,
  month_names = format(ISOdate(2000, 1:12, 1), "%B"),
  month_abb = format(ISOdate(2000, 1:12, 1), "%b")
){
  # preconditions
  stopifnot(
    is_scalar_character(format),
    is.character(month_names) && identical(length(month_names), 12L),
    is.character(month_abb) && identical(length(month_abb), 12L)
  )


  # degenerate cases
    if (identical(length(x), 0L))  return(character())


  # tokenize
  tokens <- tokenize_format(format, valid_tokens = valid_tokens)

  if ("%W" %in% tokens){
    tokens[tokens == "%W"] <- "%V"
    .Deprecated(msg = paste(
      "Please use '%V' instead of '%W'. Using '%W'",
      "for isoweeks will be disabled in future version of dint",
      "to prevent confusion with strftime() where it denotes differently defined UK-weeks")
    )
  }


  len  <- length(tokens)
  res  <- vector("list", length(tokens))
  year <- get_year(x)
  yr   <- sign(year) * (abs(year) %% 100L)


  for(i in seq_len(len)){
    if (identical(tokens[[i]], "%%"))
      res[[i]] <- "%"
    else if (identical(tokens[[i]], "%Y"))
      res[[i]] <- year
    else if (identical(tokens[[i]], "%y"))
      res[[i]] <- pad_zero_left(yr)
    else if (identical(tokens[[i]], "%q"))
      res[[i]] <- get_quarter(x)
    else if (identical(tokens[[i]], "%m"))
      res[[i]] <- pad_zero_left(get_month(x))
    else if (identical(tokens[[i]], "%B"))
      res[[i]] <- factor(get_month(x), levels = 1:12, labels = month_names)
    else if (identical(tokens[[i]], "%b"))
      res[[i]] <- factor(get_month(x), levels = 1:12, labels = month_abb)
    else if (identical(tokens[[i]], "%V"))
      res[[i]] <- pad_zero_left(get_isoweek(x))
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
  x,
  valid_tokens = NULL
){
  pos <- unlist(gregexpr("%.", x))

  if (identical(pos, -1L))
    return(x)
  pos <- sort(unique(c(1L, pos, pos + 2L, nchar(x) + 1L)))
  res <- vector("character", length(x))
  begin <- 1L
  for(i in seq_len(length(pos) -1L)) {
    res[[i]] <- substr(x, pos[[i]], pos[[i + 1]] - 1L)
  }

  if (!is.null(valid_tokens)){
    placeholders <- grep("%", res, value = TRUE)
    assert(
      all(placeholders %in% valid_tokens),
      "'format' contains unrecognised format specifications: ",
      paste(sort(setdiff(placeholders, valid_tokens)), collapse = ", ")
    )
  }

  res
}




pad_zero_left <- function(x){
  sign <- ifelse(x < 0, "-", "")
  ifelse(nchar(abs(x)) == 1, paste0(sign, "0", abs(x)), x)
}




# shortcuts ---------------------------------------------------------------

#' Coerce and Format to Year-Quarter Strings
#'
#' @param x,q Two integer (vectors). `q` is optional and the interpretation of
#'   `x` will depend on whether `q` is supplied or not:
#'   * if only `x` is supplied, `x` will be passed to [as_date_yq()]
#'     (e.g. `x = 20161` means first quarter of 2016)
#'   * if `x` and `q` are supplied, `x` is interpreted as year and `q` as
#'     quarter.
#'
#' @inherit format.date_yq
#'
#' @family coerce and format functions
#' @seealso [format.date_yq()]
#' @export
#' @examples
#'
#' format_yq(2015, 1)
#' format_yq(20151, format = "short")
#' format_yq(20151, format = "shorter")
#'
format_yq <- function(x, q = NULL, format = "%Y-Q%q"){
  if (is.null(q)){
    d <- as_date_yq(x)
  } else {
    d <- date_yq(x, q)
  }

  format(d, format = format)
}




#' Coerce and Format to Year-Month Strings
#'
#' @param x,m Two integer (vectors). `m` is optional and the interpretation of
#'   `x` will depend on whether `m` is supplied or not:
#'   * if only `x` is supplied, `x` will be passed to [as_date_ym()]
#'     (e.g. `x = 201604` means April 2016)
#'   * if `x` and `m` are supplied, `x` is interpreted as year and `m` as
#'     month.
#'
#' @inherit format.date_ym
#'
#' @family coerce and format functions
#' @seealso [format.date_ym()]
#' @export
#' @examples
#'
#' format_ym(2015, 5)
#' format_ym(201505, format = "short")
#' format_ym(201505, format = "shorter")
#'
format_ym <- function(x, m = NULL, format = "%Y-M%m"){
  if (is.null(m)){
    d <- as_date_ym(x)
  } else {
    d <- date_ym(x, m)
  }

  format(d, format = format)
}




#' Coerce and Format to Year-Isoweek Strings
#'
#' @param x,w Two integer (vectors). `w` is optional and the interpretation of
#'   `x` will depend on whether `w` is supplied or not:
#'   * if only `x` is supplied, `x` will be passed to [as_date_yw()]
#'     (e.g. `x = 201604` means 4th isoweek of 2016)
#'   * if `x` and `w` are supplied, `x` is interpreted as year and `w` as
#'     week.
#'
#' @inherit format.date_yw
#'
#' @family coerce and format functions
#' @seealso [format.date_yw()]
#' @export
#' @examples
#' format_yw(2015, 5)
#' format_yw(201505, format = "%Y.%V")
#' format_yw(as_date_yw(201505), format = "%y.%V")
#'
format_yw <- function(
  x,
  w = NULL,
  format = "%Y-W%V"
){
  if (is.null(w)){
    d <- as_date_yw(x)
  } else {
    d <- date_yw(x, w)
  }

  format(d, format = format)
}




# more shortcuts ---------------------------------------------------------

#' @rdname format_date_xx
#' @export
format_yq_iso <- function(x){
  format(as_date_yq(x))
}




#' @rdname format_date_xx
#' @export
format_yq_short   <- function(x){
  format(as_date_yq(x), "%Y.%q")
}




#' @rdname format_date_xx
#' @export
format_yq_shorter <- function(x){
  format(as_date_yq(x), "%y.%q")
}




#' @rdname format_date_xx
#' @export
format_ym_iso <- function(x){
  format(as_date_ym(x))
}




#' @rdname format_date_xx
#' @export
format_ym_short   <- function(x){
  format(as_date_ym(x), "%Y.%m")
}




#' @rdname format_date_xx
#' @export
format_ym_shorter <- function(x){
  format(as_date_ym(x), "%y.%m")
}




#' @rdname format_date_xx
#' @export
format_yw_iso <- function(x){
  format(as_date_yw(x))
}




#' @rdname format_date_xx
#' @export
format_yw_short <- function(x){
  format(as_date_yw(x), "%Y.%V")
}




#' @rdname format_date_xx
#' @export
format_yw_shorter <- function(x){
  format(as_date_yw(x), "%y.%V")
}
