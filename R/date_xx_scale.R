#' @include zoo-yearqtr.R
#' @include utils-sfmisc.R

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
  assert_namespace("ggplot2")


  ggplot2::continuous_scale(
    aesthetics,
    "Month",
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
  padded = c(0, 0)
){
  assert(is_integerish(padded) && length(padded) %in% 1:2)
  assert(is_scalar_integerish(n))

  if (length(padded == 1)){
    padded <- c(padded, padded)
  }

  function(x){

    if (all(is.na(x)))  return(x)
    x <- as_date_yq(x)
    xmin <- min(x, na.rm = TRUE)
    xmax <- max(x, na.rm = TRUE)
    diff <- (xmax - xmin)

    if (diff <= n){
      breaks <- seq(xmin, xmax)


    } else if (diff < 12){
      by <- as.integer((ceiling(diff/n/2) * 2))

      breaks <- seq(
        date_yq(get_year(xmin), 1L),
        date_yq(get_year(xmax), 4L),
        by = by
      )

    } else {
      ymin <- get_year(xmin)
      ymax <- get_year(xmax + 1L)
      diff <- ymax - ymin
      by <- as.integer(ceiling(diff/n))
      breaks <- date_yq(seq(ymin, ymax, by = by), 1)
    }

    # fix breaks at the corner of the plot (outside the data range)
    # this works well if the plot area is padded by 1 unit
    # (see scale_date_** expand argument)
    breaks <- breaks[breaks > xmin & breaks < xmax]

    if (length(breaks) == 1){
      d <- min(breaks - xmin, xmax - breaks)
      breaks <- unique(c(breaks - d, breaks, breaks + d))
    }

    breaks
  }
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
date_ym_breaks <- function(
  n = 6,
  padded = c(0, 0)
){
  assert(is_integerish(padded) && length(padded) %in% 1:2)
  assert(is_scalar_integerish(n))

  if (length(padded == 1)){
    padded <- c(padded, padded)
  }

  function(x){

    if (all(is.na(x)))  return(x)
    x <- as_date_ym(x)
    xmin <- min(x, na.rm = TRUE)
    xmax <- max(x, na.rm = TRUE)
    diff <- (xmax - xmin)

    if (diff <= n){
      breaks <- seq(xmin, xmax)


    } else if (diff < 24){
      by <- as.integer((ceiling(diff/n/3) * 3))

      breaks <- seq(
        date_ym(get_year(xmin), 1L),
        date_ym(get_year(xmax), 12L),
        by = by
      )

    } else {
      ymin <- get_year(xmin)
      ymax <- get_year(xmax + 1L)
      diff <- ymax - ymin
      by <- as.integer(ceiling(diff/n))

      breaks <- date_ym(seq(ymin, ymax, by = by), 1)
    }

    # fix breaks at the corner of the plot (outside the data range)
    # this works well if the plot area is padded by 1 unit
    # (see scale_date_** expand argument)
    breaks <- breaks[breaks > xmin & breaks < xmax]

    if (length(breaks) == 1){
      d <- min(breaks - xmin, xmax - breaks)
      breaks <- unique(c(breaks - d, breaks, breaks + d))
    }

    breaks
  }
}





# transformations ---------------------------------------------------------

if (requireNamespace("scales", quietly = TRUE)){
  date_yq_trans <- scales::trans_new(
    name = "date_yq",
    transform = as_yearqtr.date_yq,
    inverse   = function(x){
      x <- round_frac(as.numeric(x), 4)
      as_date_yq.yearqtr(x)
    },
    breaks = date_yq_breaks(padded = 1L),
    format = function(x){
      if (all(get_quarter(x) == 1L | is.na(x))){
        as.character(get_year(x))
      } else {
        format_yq_short(x)
      }
    }
  )



  date_ym_trans <- scales::trans_new(
    name = "date_ym",
    transform = as_yearmon.date_ym,
    inverse   = function(x){
      x <- round_frac(as.numeric(x), 12)
      as_date_ym.yearmon(x)
    },
    breaks = date_ym_breaks(padded = 1L),
    format = function(x){
      if (all(get_month(x) == 1L | is.na(x))){
        as.character(get_year(x))
      } else {
        format_ym_short(x)
      }
    }
  )

}



round_frac <- function(
  x,
  denom
){
  whole <- x %/% 1
  frac  <- x %% 1
  whole + round(frac * denom) / denom
}

