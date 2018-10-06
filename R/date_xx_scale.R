#' @rdname scale_date
#' @export
scale_y_date_xx <- function(
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  oob = censor,
  na.value = NA_real_,
  position = "left",
  sec.axis = waiver()
){
  scale_y_continuous(
    name = name,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    position = position,
    trans = scales::hms_trans(),
    sec.axis = sec.axis
  )
}


#' Title
#'
#' @param n
#'
#' @return
#' @export
#'
#' @examples
date_yq_breaks <- function(
  n = 5
){


  assert_namespace("labeling")
  force(n)
  function(x) {
    rng_y  <- range(get_year(x))
    diff_y <- diff(rng_y)

    if (diff_y > 3){
      rng <- c(rng_y[[1]], rng_y[[2]] + 1)
    } else {
      rng <- as_yeartqtr.date_yq(range(x))
    }

    breaks <- labeling::extended(
      rng_y[1],
      rng_y[2],
      n,
      Q = c(1, 1.25, 1.5, 1.75),
      only.loose = FALSE
    )

    breaks <- yearqtr(breaks)
    as_date_yq(breaks)
  }
}



date_xx_scale <- function(
  aesthetics,
  trans,
  palette,
  breaks = pretty_breaks(),
  minor_breaks = waiver(),
  labels = waiver(),
  date_breaks = waiver(),
  date_labels = waiver(),
  date_minor_breaks = waiver(),
  timezone = NULL,
  guide = "legend",
  ...
){
  # Backward compatibility
  if (is.character(breaks)) breaks <- date_breaks(breaks)
  if (is.character(minor_breaks)) minor_breaks <- date_breaks(minor_breaks)

  if (!is.waive(date_breaks)) {
    breaks <- date_breaks(date_breaks)
  }
  if (!is.waive(date_minor_breaks)) {
    minor_breaks <- date_breaks(date_minor_breaks)
  }
  if (!is.waive(date_labels)) {
    labels <- function(self, x) {
      tz <- if (is.null(self$timezone)) "UTC" else self$timezone
      date_format(date_labels, tz)(x)
    }
  }

  name <- switch(
    trans,
    date = "date",
    time = "datetime"
  )

  # x/y position aesthetics should use ScaleContinuousDate or
  # ScaleContinuousDatetime; others use ScaleContinuous
  if (all(aesthetics %in% c("x", "xmin", "xmax", "xend", "y", "ymin", "ymax", "yend"))) {
    scale_class <- switch(
      trans,
      date = ScaleContinuousDate,
      time = ScaleContinuousDatetime
    )
  } else {
    scale_class <- ScaleContinuous
  }

  sc <- continuous_scale(
    aesthetics,
    name,
    palette = palette,
    breaks = breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    guide = guide,
    trans = trans,
    ...,
    super = scale_class
  )
  sc$timezone <- timezone
  sc
}




as_date_yq.yearqtr <- function(
  x
){
  x <- as.numeric(x)
  assert(all(x > 0))

  tx  <- trunc(x)
  rem <- x - tx

  assert(all(rem %in% c(0, 0.25, 0.5, 0.75)))
  date_yq(tx,  (x - tx) * 4 + 1L )
}


yearqtr <- function(x){
  structure(x, class = c("yearqtr", "numeric"))
}


as_yeartqtr.date_yq <- function(x){
  get_year(x) + (get_quarter(x) - 1L) / 4
}



