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
  x,
  each = NULL
){
  force(each)

  function(x){
    if (is.null(each)){
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
    } else {
      stop("not yet supported")
    }
  }
}


