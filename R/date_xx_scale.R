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
  ggplot2:::continuous_scale(
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


#' Title
#'
#' @param n
#'
#' @return
#' @export
#'
#' @examples
date_yq_breaks <- function(
  x
){
    x <- as_date_yq(x)

    xmin <- min(x)
    xmax <- max(x)

    nyears <- get_year(xmax) - get_year(xmin)

    if (nyears <= 2L){
      return(seq(xmin + 1L, xmax- 1L))
    }
    else if (nyears == 3){
      xmin <- xmin + 1L
      xmax <- xmax - 1L
      if (get_quarter(xmin) %in%  c(2, 4))  xmin <- xmin + 1L

      return(seq(xmin, xmax, by = 2))

    } else {

      if (get_quarter(xmax) == 1L) xmax <- xmax - 1L

      each <- round((get_year(xmax) - get_year(xmin)) / 6)
      seq(ceiling(xmin), floor(xmax), by = 4 * each)
    }
}



as_date_yq.yearqtr <- function(
  x
){
  x <- as.numeric(x)
  assert(all(x > 0 | is.na(x)))

  tx  <- trunc(x)
  rem <- x - tx

  assert(all(rem %in% c(0, 0.25, 0.5, 0.75)))

  date_yq(tx,  (x - tx) * 4 + 1L )
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_yeartqtr <- function(x){
  UseMethod("as_yeartqtr")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
yearqtr <- function(x){
  structure(x, class = c("yearqtr", "numeric"))
}



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_yeartqtr.date_yq <- function(x){
  yearqtr(get_year(x) + (get_quarter(x) - 1L) / 4)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_yeartqtr.yearqtr <- function(x){
  x
}
