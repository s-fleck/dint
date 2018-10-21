#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_x_date_yq <- function(...) {
  scale_date_yq(aesthetics = c("x", "xmin", "xmax", "xend"), ...)
}




#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_y_date_yq <- function(...) {
  scale_date_yq(aesthetics = c("y", "ymin", "ymax", "yend"), ...)
}




#' @rdname scale_date
#' @export
scale_date_yq <- function(
  aesthetics,
  position = "bottom",
  limits = NULL,
  ...
){
  ggplot2::continuous_scale(
    aesthetics,
    "Quarter",
    identity,
    guide = "none",
    trans = date_yq_trans,
    super = ggplot2::ScaleContinuousDate,
    position = position,
    limits = limits,
    expand = c(0, 0.25, 0, 0.25), # add on quarter on each side
    ...
  )
}



# breaks ------------------------------------------------------------------

#' Pretty breaks for date_yq vectors
#'
#' @param n `NULL` or `integer` scalar
#'
#' @return a `function` that calculates a maximum of `n` breaks for a  `date_yq`
#'   vector
#' @export
#'
#' @examples
date_yq_breaks <- function(
  n = 6,
  padded = c(0L, 0L)
){
  date_xx_breaks_impl(
    n = n,
    padded = padded,
    period = 4L,
    as_dint = as_date_yq,
    get_subunit = get_quarter
  )

}




#' Pretty breaks for date_yq vectors
#'
#' @param n `NULL` or `integer` scalar
#' @param padded
#' @param period `scalar` integer. number of units in a year (i.e 4 for
#'   quarters, 12 for months, ...)
#' @param as_dint
#' @param get_subunit
#'
#' @return a `function` that calculates a maximum of `n` breaks for a  `date_yq`
#'   vector
#' @export
#'
#' @examples
date_xx_breaks_impl <- function(
  n = 6,
  padded = c(0L, 0L),
  period = 12L,
  as_dint = as_date_ym,
  get_subunit = get_month
){
  assert(is_integerish(padded) && length(padded) %in% 1:2)
  assert(is_scalar_integerish(n))

  if (length(padded == 1)){
    padded <- c(padded, padded)
  }

  function(x){
    if (all(is.na(x)))  return(x)

    x <- as_dint(x)
    xmin <- min(x, na.rm = TRUE) + padded[[1]]
    xmax <- max(x, na.rm = TRUE) - padded[[2]]
    diff <- (xmax - xmin)


    if (diff < n){
      breaks <- seq(xmin, xmax)


    } else if (diff < 3 * n){

      contbreaks <-
        labeling::extended(as_yearqtr(xmin), as_yearqtr(xmax), m = n, Q = c(0, 0.5))
        breaks <- as_date_yq(yearqtr(contbreaks))

      } else {
        ymin <- get_year(ceiling(xmin))
        ymax <- get_year(ceiling(xmax))

        yrbreaks <- labeling::extended(ymin, ymax, m = n)

        # attempt to produce nicer breaks in case corner breaks fall outside
        # the printable range
        if (yrbreaks[[1]] < ymin || yrbreaks[length(yrbreaks)] > ymax ){
          yrbreaks <- labeling::extended(ymin, ymax, m = n + 2L)
        }

        yrbreaks <- as.integer(yrbreaks)
        breaks <- date_yq(yrbreaks, 1L)
      }

    # fix breaks at the corner of the plot (outside the data range)
    # this works well if the plot area is padded by 1 unit
    # (see scale_date_** expand argument)
    if (breaks[[1]] < xmin) breaks <- breaks[-1L]
    if (breaks[[length(breaks)]] > xmax) breaks <- breaks[-length(breaks)]
    if (length(breaks) == 1){
      d <- min(breaks - xmin, xmax - breaks)
      breaks <- unique(c(breaks - d, breaks, breaks + d))
    }

    breaks
  }
}


