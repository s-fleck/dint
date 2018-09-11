#' @rdname date_xx_arithmetic
#' @export
seq.date_yw <- function(
  from,
  to,
  by = "1 week",
  ...
){
  assert(is_date_yw(to))

  if (!is.null(by)){
    by_p <- parse_seq_by(by)
    unit <- attr(by_p, "unit")
  }

  if (identical(unit, "isoyear")){
    w_from <- get_isoweek(from)
    w_to   <- get_isoweek(to)
    years  <- seq(get_isoyear(from), get_isoyear(to) - as.integer(w_from > w_to), by = by_p)
    return(as_date_yw(years * 100 + get_isoweek(from)))

  } else {
    as_date_yw(seq(first_of_isoweek(from), first_of_isoweek(to), by = by))
  }
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_yq <- function(from, to, ...){
  res <- seq.int(as.integer(from), as.integer(to))
  as_date_yq(res[(res %% 10L) %in% 1:4])
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_ym <- function(from, to, ...){
  res <- seq.int(as.integer(from), as.integer(to))
  as_date_ym(res[(res %% 100) %in% 1:12])
}




# parse by ----------------------------------------------------------------

parse_seq_by <- function(
  by
){
  assert(
    is_scalar(by),
    "'by' must be a scalar (a vector of length 1)"
  )

  if (is.integer(by))
    return (by)

  if (is.numeric(by)){
    ret <- as.integer(by)
    if (ret == by)
      return(ret)
    else
      stop("If 'by' is a number, it must be an integer")
  }


  if (is.character(by)){
    by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]

    if (identical(length(by2), 1L)){
      ret <- 1L
      unit <- by2
    } else {
      ret <- as.integer(by2[[1]])
      assert(
        identical(ret == by2[[1]], TRUE),
        "If 'by' is a number, it must be an integer"
      )
      unit <- by2[[2]]
    }

    unit <- gsub("s$", "", unit)
    valid_units <- c("year", "quarter", "month", "week", "isoyear")
    assert(
      unit %in% valid_units,
      msg = sprintf(
        "'%s' is not a valid unit. Valid units are: %s (optionally followed by an 's')",
        unit, paste(sprintf("'%s'", valid_units), collapse = ", ")
      )
    )

    return(structure(
      ret,
      unit = unit
    ))
  }
}
