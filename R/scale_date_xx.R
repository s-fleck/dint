# scale_date_yq -----------------------------------------------------------

#' ggplot2 Scales For date_xx Objects
#'
#' The `scale_*_date_**` functions provide nice defaults for plotting
#' the appropriate [date_xx] subclass, but come with a limited number of
#' configuration options. If you require more finetuning, you can convert
#' [date_xx] vectors with [as.Date()] and use [ggplot2::scale_x_date()].
#'
#'
#' @inheritParams ggplot2::scale_x_date
#' @seealso [date_xx_breaks]
#'
#' @name scale_date_xx
#' @include zoo-compat.R
#' @include utils-sfmisc.R
#' @include first_of.R
#' @include format.R
#'
#' @examples
#' if (require("ggplot2", quietly = TRUE)){
#'   dd <- data.frame(date = seq(date_yq(2016, 1), date_yq(2018, 1)), V1 = 1:9)
#'   p <- ggplot(dd, aes(x = date, y = V1)) +
#'     geom_point()
#'
#'   p  # automatically uses the proper scale
#'   p + scale_x_date_yq("quarters with default spacing")
#'   p + scale_x_date_yq(breaks = date_yq_breaks(3))
#' }
#'
NULL




# This tells ggplot2 what scale to look for
scale_type.date_yq <- function(x) "date_yq"
scale_type.date_ym <- function(x) "date_ym"
scale_type.date_yw <- function(x) "date_yw"




#' @rdname scale_date_xx
#' @export
scale_x_date_yq <- function(
  name = "Quarter",
  breaks = date_yq_breaks(),
  limits = NULL,
  position = "bottom"
){
  scale_date_yq(
    aesthetics = c("x", "xmin", "xmax", "xend"),
    name = name,
    limits = limits,
    breaks = breaks,
    position = position
  )
}




#' @rdname scale_date_xx
#' @export
scale_y_date_yq <- function(
  name = "Quarter",
  breaks = date_yq_breaks(),
  limits = NULL,
  position = "left"
){
  scale_date_yq(
    aesthetics = c("y", "ymin", "ymax", "yend"),
    name = name,
    limits = limits,
    breaks = breaks,
    position = position
  )
}




scale_date_yq <- function(
  aesthetics,
  name = "Quarter",
  breaks = date_yq_breaks(),
  limits = NULL,
  position = "bottom"
){
  ggplot2::continuous_scale(
    aesthetics,
    scale_name = "date_yq",
    name = name,
    palette = identity,
    guide = "none",
    trans = date_yq_trans,
    super = ggplot2::ScaleContinuousDate,
    position = position,
    limits = limits,
    breaks = breaks,
    expand = c(0.04, 0)
  )
}




# date_ym -----------------------------------------------------------------

#' @rdname scale_date_xx
#' @export
scale_x_date_ym <- function(
  name = "Month",
  breaks = date_ym_breaks(),
  limits = NULL,
  position = "bottom"
){
  scale_date_ym(
    aesthetics = c("x", "xmin", "xmax", "xend"),
    name = name,
    breaks = breaks,
    limits = limits,
    position = position
  )
}




#' @rdname scale_date_xx
#' @export
scale_y_date_ym <- function(
  name = "Month",
  breaks = date_ym_breaks(),
  limits = NULL,
  position = "left"
) {
  scale_date_ym(aesthetics = c(
    "y", "ymin", "ymax", "yend"),
    name = name,
    breaks = breaks,
    limits = limits,
    position = position
  )
}




scale_date_ym <- function(
  aesthetics,
  name = "Month",
  breaks = date_ym_breaks(),
  limits = NULL,
  position = "left"
){
  ggplot2::continuous_scale(
    aesthetics,
    scale_name = "date_ym",
    name = name,
    breaks = breaks,
    palette = identity,
    guide = "none",
    trans = date_ym_trans,
    super = ggplot2::ScaleContinuousDate,
    position = position,
    limits = limits,
    expand = c(0.04, 0)
  )
}


# date_yw -----------------------------------------------------------------

#' @rdname scale_date_xx
#' @export
scale_x_date_yw <- function(
  name = "Month",
  breaks = date_yw_breaks(),
  limits = NULL,
  position = "bottom"
){
  scale_date_yw(
    aesthetics = c("x", "xmin", "xmax", "xend"),
    name = name,
    breaks = breaks,
    limits = limits,
    position = position
  )
}




#' @rdname scale_date_xx
#' @export
scale_y_date_yw <- function(
  name = "Month",
  breaks = date_yw_breaks(),
  limits = NULL,
  position = "left"
){
  scale_date_yw(
    aesthetics = c("y", "ywin", "ywax", "yend"),
    name = name,
    breaks = breaks,
    limits = limits,
    position = position
  )
}




scale_date_yw <- function(
  aesthetics,
  name = "Week",
  breaks = date_yw_breaks(),
  limits = NULL,
  position = "bottom"
){
  ggplot2::continuous_scale(
    aesthetics,
    scale_name = "date_yw",
    name = name,
    breaks = breaks,
    palette = identity,
    guide = "none",
    trans = date_yw_trans,
    super = ggplot2::ScaleContinuousDate,
    position = position,
    limits = limits,
    expand = waiver()
  )
}




# breaks ------------------------------------------------------------------

#' Pretty Breaks For date_xx Vectors
#'
#' `date_*_breaks` does not return breaks, but a function that calculates
#' breaks. This is for compatbility with the breaks functions from \pkg{scales}
#' such as [scales::pretty_breaks()], and for ease of use with \pkg{ggplot2}.
#'
#' @param n `NULL` or `integer` scalar. The desired maximum number of breaks.
#' The breaks algorithm may choose less breaks if it sees fit.
#'
#' @return a `function` that calculates a maximum of `n` breaks for a `date_xx`
#'   vector
#'
#' @name date_xx_breaks
#' @examples
#' x <- date_ym(2016, 1:12)
#' date_ym_breaks()(x)
#' date_ym_breaks(12)(x)
NULL




#' @name date_xx_breaks
#' @export
date_yq_breaks <- function(
  n = 6
){
  assert(is_scalar_integerish(n))
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
      breaks <- breaks[breaks > xmin & breaks < xmax]
    }

    # fix breaks at the corner of the plot (outside the data range)
    # this works well if the plot area is padded by 1 unit
    # (see scale_date_** expand argument)
    if (length(breaks) == 1){
      d <- min(breaks - xmin, xmax - breaks)
      breaks <- unique(c(breaks - d, breaks, breaks + d))
    }

    breaks
  }
}




#' @rdname date_xx_breaks
#' @export
date_ym_breaks <- function(
  n = 6
){
  assert(is_scalar_integerish(n))
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

    breaks <- breaks[breaks >= xmin & breaks <= xmax]

    if (length(breaks) == 1){
      d <- min(breaks - xmin, xmax - breaks)
      breaks <- unique(c(breaks - d, breaks, breaks + d))
    }

    breaks
  }
}




#' @rdname date_xx_breaks
#' @export
date_yw_breaks <- function(
  n = 6
){
  assert(is_scalar_integerish(n))

  function(x){

    if (all(is.na(x)))  return(x)
    x <- as_date_yw(x)
    xmin <- min(x, na.rm = TRUE)
    xmax <- max(x, na.rm = TRUE)
    diff <- (xmax - xmin)

    if (diff <= n){
      breaks <- seq(xmin, xmax)


    } else if (diff < 53){
      by <- as.integer((ceiling(diff/n/4) * 4))

      breaks <- seq(
        as_date_yw(first_of_isoyear(xmin)),
        as_date_yw(last_of_isoyear(xmax)),
        by = by
      )

    } else if (diff < 106){
      by <- as.integer((ceiling(diff/n/13) * 13))

      breaks <- seq(
        as_date_yw(first_of_isoyear(xmin)),
        as_date_yw(last_of_isoyear(xmax)),
        by = by
      )

    } else {
      ywin <- get_year(xmin)
      ywax <- get_year(xmax + 1L)
      diff <- ywax - ywin
      by <- as.integer(ceiling(diff/n))

      breaks <- date_yw(seq(ywin, ywax, by = by), 1)
    }

    # fix breaks at the corner of the plot (outside the data range)
    # this works well if the plot area is padded by 1 unit
    # (see scale_date_** expand argument)
    breaks <- breaks[breaks >= xmin & breaks <= xmax]

    if (length(breaks) == 1){
      d <- min(breaks - xmin, xmax - breaks)
      breaks <- unique(c(breaks - d, breaks, breaks + d))
    }

    breaks
  }
}




# utils -------------------------------------------------------------------

#' Round to Fraction
#'
#' eg `1/4` for `0`, `0.25`, `0.5`, `0.75`
#'
#' @param x a `numeric` vector
#' @param denom a `numeric` scalar (e.g 4 for 1/4)
#'
#' @return a `numeric` vector
#' @noRd
#'
round_frac <- function(
  x,
  denom
){
  (x %/% 1) + round((x %% 1) * denom) / denom
}
