#' Parse Dates With Year and Quarter Components
#'
#' These are generic parsers for year/quarter/month formats that work with
#' nearly all possible year/quarter formats. The only prerequisits are that
#' `x` contains a 4-digit-year and a 1-digit-quarter or 2-digit-month and no
#' additional numbers.
#'
#' @param x a `character` vector
#' @param quiet a `logical` scalar. If `TRUE` warnings on parsing failures are
#'   suppressed.
#'
#' @return a `date_yq` or `date_ym` vector
#' @export
#'
#' @examples
#' yq("2018 1")
#' qy("1st Quarter 2019")
#'
#' #' # Works even for filenames, as long as they contain no additional numbers
#' yq("business_report-2018_1.pdf")
#' my("business_report-082018.pdf")
yq <- function(x, quiet = FALSE){
  assert(is.character(x), "'x' must be a character vector")
  assert(is_scalar_bool(quiet))

  r <- vapply(x, parse_yq, integer(1), pattern = "^[^0-9]*\\d{4}[^0-9]*[1-4][^0-9]*$", USE.NAMES = FALSE)

  if (!quiet){
    failed <- sum(is.na(r)) - sum(is.na(x))
    if (failed > 0){
      warning(failed, " failed to parse.")
    }
  }

  as_date_yq(r)
}




#' @rdname yq
#' @export
qy <- function(x, quiet = FALSE){
  assert(is.character(x), "'x' must be a character vector")
  r <- vapply(x, parse_yq, integer(1), pattern = "^[^0-9]*[1-4][^0-9]*\\d{4}[^0-9]*$", USE.NAMES = FALSE)

  if (!quiet){
    failed <- sum(is.na(r)) - sum(is.na(x))
    if (failed > 0){
      warning(failed, " failed to parse.")
    }
  }

  as_date_yq(r)
}




#' @rdname yq
#' @export
ym <- function(x, quiet = FALSE){
  assert(is.character(x), "'x' must be a character vector")
  assert(is_scalar_bool(quiet))

  r <- vapply(x, parse_ym, integer(1), pattern = "^[^0-9]*\\d{4}[^0-9]*((0[1-9])|11|12)[^0-9]*$", USE.NAMES = FALSE)

  if (!quiet){
    failed <- sum(is.na(r)) - sum(is.na(x))
    if (failed > 0){
      warning(failed, " failed to parse.")
    }
  }


  as_date_ym(r)
}




#' @rdname yq
#' @export
my <- function(x, quiet = FALSE){
  assert(is.character(x), "'x' must be a character vector")
  r <- vapply(x, parse_my, integer(1), pattern = "^[^0-9]*((0[1-9])|11|12)[^0-9]*\\d{4}[^0-9]*$", USE.NAMES = FALSE)

  if (!quiet){
    failed <- sum(is.na(r)) - sum(is.na(x))
    if (failed > 0){
      warning(failed, " failed to parse.")
    }
  }

  as_date_ym(r)
}




#' @param x a `character` scalar
#' @param pattern a `character` scalar. If `x` does not match `pattern`,
#'   `NA_integer` is returned
#'
#' @return an `integer` scalar for as_date_yq
#' @noRd
parse_yq <- function(x, pattern){
  if (!grepl(pattern, x)){
    return(NA_integer_)
  }

  extr <- function(string, pos, length){
    if (identical(pos, -1))
      NA_integer_
    else
      as.integer(substr(string, pos, pos + length))
  }

  pos_y <- regexpr("\\d{4}", x)
  year  <- extr(x, pos_y, 3)
  x     <- gsub("\\d{4}", "", x)

  pos_q <- regexpr("[1-4]{1}[^0-9]*", x)
  quarter <- extr(x, pos_q, 0)

  as.integer(year) * 10L + as.integer(quarter)
}




#' @inheritParams parse_yq
#' @return an `integer` scalar for as_date_ym
#' @noRd
parse_ym <- function(x, pattern){
  if (!grepl(pattern, x)){
    return(NA_integer_)
  }

  extr <- function(string, pos, length){
    if (identical(pos, -1))
      NA_integer_
    else
      as.integer(substr(string, pos, pos + length))
  }

  pos_m <- regexpr("((0[1-9])|11|12)[^0-9]*$", x)
  month <- extr(x, pos_m, 1)
  x <- strtrim(x, pos_m - 1L)


  pos_y <- regexpr("\\d{4}", x)
  year  <- extr(x, pos_y, 3)

  as.integer(year) * 100L + as.integer(month)
}




#' @inheritParams parse_yq
#' @return an `integer` scalar for as_date_ym
#' @noRd
parse_my <- function(x, pattern){
  if (!grepl(pattern, x)){
    return(NA_integer_)
  }

  extr <- function(string, pos, length){
    if (identical(pos, -1))
      NA_integer_
    else
      as.integer(substr(string, pos, pos + length))
  }

  pos_y <- regexpr("\\d{4}[^0-9]*$", x)
  year  <- extr(x, pos_y, 3)
  x <- strtrim(x, pos_y - 1L)

  pos_m <- regexpr("(0[1-9])|11|12", x)
  month <- extr(x, pos_m, 1)

  as.integer(year) * 100L + as.integer(month)
}
