#' Parse Dates With Year and Quarter Components
#'
#' These are very generic parsers for year/quarter formats that work with
#' nearly all possible year/quarter formats. As long as `x` contains a
#' 4-digit-year and a 1-digit-quarter and no additional numbers,
#' you should be fine.
#'
#' @param x a `character` vector
#' @param quiet a `logical` scalar. If `TRUE` warnings on parsing failures are
#'   suppressed.
#'
#' @return a `date_yq` vector
#' @export
#'
#' @examples
#' yq("2018 1")
#' qy("1st Quarter 2019")
#'
#' #' # Works even for filenames, as long as they contain no additional numbers
#' yq("business_report-2018_1.pdf")
yq <- function(x, quiet = FALSE){
  assert(is.character(x), "'x' must be a character vector")
  assert(is_scalar_bool(quiet))

  r <- vapply(x, parse_yq, integer(1), pattern = "^[^0-9]*\\d{4}[^0-9]*\\d[^0-9]*$", USE.NAMES = FALSE)

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
  r <- vapply(x, parse_yq, integer(1), pattern = "^[^0-9]*\\d[^0-9]*\\d{4}[^0-9]*$", USE.NAMES = FALSE)

  if (!quiet){
    failed <- sum(is.na(r)) - sum(is.na(x))
    if (failed > 0){
      warning(failed, " failed to parse.")
    }
  }

  as_date_yq(r)
}




#' @param x a `character` scalar
#' @param pattern a `character` scalar. Accepted patterns in X
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
