#' Increment functions for various self-defined objects
#'
#' Function to increment objects
#'
#' @param x object to increment
#' @param inc Value by which to increment (usually integer)
#'
#' @return An object of the same type increment by inc
#' @rdname increment
#' @export
increment <- function(x, inc = 1){
  UseMethod("increment", x)
}




#' @export
#' @rdname increment
increment.date_yq <- function(x, inc){
  d <- yqs_matrix_from_numeric(x)
  d[, 2] <- d[, 2] + inc

  y_add <- d[, 2] %/% 4
  q_new <- d[, 2] %% 4

  sel <- q_new == 0
  y_add[sel] <- y_add[sel] - 1L
  q_new[sel] <- 4L

  d[, 1] <- d[, 1] + y_add * d[, 3]
  d[, 2] <- q_new


  date_yq(d[, 1] * d[, 3], d[, 2])
}




#' @export
#' @rdname increment
increment.date_ym <- function(x, inc){
  d <- yms_matrix_from_numeric(x)
  d[, 2] <- d[, 2] + inc

  y_add <- d[, 2] %/% 12
  q_new <- d[, 2] %% 12

  sel <- q_new == 0
  y_add[sel] <- y_add[sel] - 1L
  q_new[sel] <- 12L

  d[, 1] <- d[, 1] + y_add * d[, 3]
  d[, 2] <- q_new


  date_ym(d[, 1] * d[, 3], d[, 2])
}
