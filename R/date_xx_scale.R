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




# date_ym -----------------------------------------------------------------

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_x_date_ym <- function(...) {
  scale_date_ym(aesthetics = c("x", "xmin", "xmax", "xend"), ...)
}




#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_y_date_ym <- function(...) {
  scale_date_ym(aesthetics = c("y", "ymin", "ymax", "yend"), ...)
}




#' @rdname scale_date
#' @export
scale_date_ym <- function(
  aesthetics,
  position = "bottom",
  limits = NULL,
  ...
){
  assert_namespace("scales")
  assert_namespace("labeling")
  assert_namespace("ggplot2")

  ggplot2::continuous_scale(
    aesthetics,
    "Quarter",
    identity,
    guide = "none",
    trans = date_ym_trans,
    super = ggplot2::ScaleContinuousDate,
    position = position,
    limits = limits,
    expand = c(0, 1/12, 0, 1/12), # add on quarter on each side
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
    date_ctor = date_yq,
    as_zoo = as_yearqtr,
    zoo_ctor = yearqtr,
    get_subunit = get_quarter
  )
}




date_ym_breaks <- function(
  n = 6,
  padded = c(0L, 0L)
){
  date_xx_breaks_impl(
    n = n,
    padded = padded,
    period = 12L,
    as_dint = as_date_ym,
    date_ctor = date_ym,
    as_zoo = as_yearmon,
    zoo_ctor = yearmon,
    get_subunit = get_month
  )
}



#' Pretty breaks for date_yq vectors
#'
#' `date_xx_breaks_impl` does not return breaks, but a function that calculates
#' breaks. This is for compatbility with the breaks functions from scales such
#' as [scales::pretty_breaks()], and for ease of use in ggplot.
#'
#'
#' @param n `NULL` or `integer` scalar
#' @param padded `integer` vector of length 1 or two. Padding to remove
#'   from each side of the input vector. This is useful for generating
#'   breaks for ggplot objects where the limits are usually padded by
#'   one month/quarter/isoweek for plotting.
#'
#' @param period `scalar` integer. number of units in a year (i.e 4 for
#'   quarters, 12 for months, ...)
#'
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
  padded,
  period,
  as_dint,
  date_ctor,
  as_zoo,
  zoo_ctor,
  get_subunit
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
        labeling::extended(
          as.numeric(as_zoo(xmin)),
          as.numeric(as_zoo(xmax)),
          m = n,
          Q = c(seq(0, 0.999, by = 1 / (period/2) ))  # 1 should not be a "pretty number"
      )

      breaks <- as_dint(zoo_ctor(contbreaks))

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
        breaks <- date_ctor(yrbreaks, 1L)
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


