#' @rdname date_xx_arithmetic
#' @export
seq.date_yw <- function(x, y, ...){
  as_date_yw(seq(first_of_isoweek(x), first_of_isoweek(y), by = "7 days"))
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_yq <- function(x, y, ...){
  res <- seq.int(as.integer(x), as.integer(y))
  as_date_yq(res[(res %% 10L) %in% 1:4])
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_ym <- function(x, y, ...){
  res <- seq.int(as.integer(x), as.integer(y))
  as_date_ym(res[(res %% 100) %in% 1:12])
}
