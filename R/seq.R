#' date_xx Sequence Generation
#'
#' @param from,to the starting and (maximal) end value of the sequence. Must be
#'   of the same class (i.e. both must be a [`date_yq`], [`date_ym`], etc..)
#' @param by a positive `integer` scalar to increment the sequence with
#'   (either in quarters, months or isoweeks, depending on the class of
#'   `from`/`to`)
#' @param ... ignored
#'
#' @return an `integer` vector with the same [`date_xx`] subclass as `from`/`to`
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
  assert(
    is_date_yw(to),
    msg = "'to' must be the same <date_xx> subclass as 'from'"
  )

  as_date_yw(seq(first_of_isoweek(from), first_of_isoweek(to), by = by * 7L * sign(as.integer(to) - as.integer(from))))
}




#' @rdname date_xx_sequences
#' @export
seq.date_yq <- function(
  from,
  to,
  by = 1L,
  ...
){
  assert(
    is_date_yq(to),
    msg = "'to' must be the same <date_xx> subclass as 'from'"
  )
  seq_date_xx(from = from, to = to, by = by, base = 4L, ctor = date_yq)
}




#' @rdname date_xx_sequences
#' @export
seq.date_ym <- function(
  from,
  to,
  by = 1L,
  ...
){
  assert(
    is_date_ym(to),
    msg = "'to' must be the same <date_xx> subclass as 'from'"
  )
  seq_date_xx(from = from, to = to, by = by, base = 12L, ctor = date_ym)
}




seq_date_xx <- function(
  from,
  to,
  by,
  base,
  ctor
){
  assert(
    is_scalar_integer(from) && !is.na(from) && from >= 0L,
    "'from' must be a positive <date_xx> scalar"
  )
  assert(
    is_scalar_integer(to) && !is.na(to) && to >= 0L,
    "'to' must be a positve <date_xx> scalar"
  )
  assert(
    is_scalar_integerish(by) && by > 0,
    "'by' must be a positive <integer> scalar"
  )
  assert(is_scalar_integer(base) && base > 0)

  rev <- from > to

  if (rev){
    tmp <- from
    from <- to
    to <- tmp
  }

  all_ms <- sort(rep.int(get_year(from):get_year(to), base))
  all_ms <- ctor(all_ms, seq_len(base))
  all_ms <- all_ms[all_ms >= from]
  all_ms <- all_ms[all_ms <= to]

  if (rev)
    all_ms <- rev(all_ms)


  all_ms[seq.int(1L, length(all_ms), by = by)]
}




# rep ---------------------------------------------------------------------

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rep.date_xx <- function(x, ...){
  asfun <- paste0("as_", which_date_xx(x))
  do.call(asfun, list(rep(as.integer(x), ...)))
}

