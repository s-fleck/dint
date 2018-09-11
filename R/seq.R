#' date_xx Sequence Generation
#'
#' @param from,to the starting and (maximal) end value of the sequence. Must be
#'   of the same class (i.e. both must be a `date_yq`, `date_ym`, etc..)
#' @param by a positive `integer` scalar to increment the sequence with
#'   (either in quarters, months or isoweeks, depending on the class of
#'   `from`/`to`)
#'
#' @name date_xx_sequences




#' @rdname date_xx_sequences
#' @export
seq.date_yw <- function(
  from,
  to,
  by = 1L,
  ...
){
  as_date_yw(seq(first_of_isoweek(from), first_of_isoweek(to), by = by * 7L))
}




#' @rdname date_xx_sequences
#' @export
seq.date_yq <- function(
  from,
  to,
  by = 1L,
  ...
){
  assert(is_date_yq(to))
  assert(is_integerish(by) && by > 0)

  rev <- from > to

  if (rev){
    tmp <- from
    from <- to
    to <- tmp
  }

  all_qs <- sort(rep.int(get_year(from):get_year(to), 4L))
  all_qs <- date_yq(all_qs, 1:4)
  all_qs <- all_qs[all_qs >= from]
  all_qs <- all_qs[all_qs <= to]

  res <- all_qs[seq.int(1L, length(all_qs), by = by)]

  if (rev)
    rev(res)
  else
    res
}



#' @rdname date_xx_sequences
#' @export
seq.date_ym <- function(from, to, ...){
  assert(is_date_ym(to))
  res <- seq.int(as.integer(from), as.integer(to))
  as_date_ym(res[(res %% 100) %in% 1:12])
}
