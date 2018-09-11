#' @rdname date_xx_arithmetic
#' @export
seq.date_yw <- function(
  from,
  to,
  by = 1L,
  ...
){
  as_date_yw(seq(first_of_isoweek(from), first_of_isoweek(to), by = by * 7L))
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_yq <- function(from, to, ...){
  assert(is_date_yq(to))
  res <- seq.int(as.integer(from), as.integer(to), by = by)
  as_date_yq(res[(res %% 10L) %in% 1:4])
}




#' @rdname date_xx_arithmetic
#' @export
seq.date_ym <- function(from, to, ...){
  assert(is_date_ym(to))
  res <- seq.int(as.integer(from), as.integer(to))
  as_date_ym(res[(res %% 100) %in% 1:12])
}
