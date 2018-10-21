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
  n = NULL,
  padded = c(0L, 0L)
){
  if (!is.null(n)){
    warning("'n' argument not yet supported")
    n <- NULL
  }

  assert(is_integerish(padded) && length(padded) %in% 1:2)

  if (length(padded == 1)){
    padded <- c(padded, padded)
  }


  force(n)
  force(padded)

  function(x){

    if (all(is.na(x))){
      return(x)
    }

    xmin <- min(x, na.rm = TRUE)
    xmax <- max(x, na.rm = TRUE)


    if (is.null(n)){
      x <- as_date_yq(x)

      nyears <- get_year(xmax) - get_year(xmin)

      if (nyears <= 2L){
        return(seq(xmin + padded[[1]], xmax - padded[[2]]))
      }
      else if (nyears == 3){
        xmin <- xmin + padded[[1]]
        xmax <- xmax - padded[[2]]
        if (get_quarter(xmin) %in%  c(2, 4))  xmin <- xmin + 1L

        return(seq(xmin, xmax, by = 2))

      } else {

        if (get_quarter(xmax) == 1L) xmax <- xmax - 1L

        each <- ceiling((get_year(xmax) - get_year(xmin)) / 6)
        seq(ceiling(xmin), floor(xmax), by = 4 * each)
      }
    } else {
      stop("something went wrong")
    }

  }
}


