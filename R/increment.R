#' Increment date_xx objects
#'
#' @param x object to increment
#' @param inc Value by which to increment (usually integer)
#'
#' @return
#'   An object of the same type as `x` increment by `inc`
#' @noRd
increment <- function(x, inc = 1){
  assert(
    is_scalar(inc) ||
    is_equal_length(x, inc) ||
    is_scalar(x),
    "When adding or subtracting from date_xx objects, both elements must ",
    "either be of the same length, or one of them must be of length one."
  )

  UseMethod("increment", x)
}




increment.date_yq <- function(x, inc){
  if (is_scalar(x) && length(inc) > 1){
    x <- rep(x, length(inc))
  }

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




increment.date_ym <- function(x, inc){
  if (is_scalar(x) && length(inc) > 1){
    x <- rep(x, length(inc))
  }

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




increment.date_yw <- function(x, inc){
  if (is_scalar(x) && length(inc) > 1){
    x <- rep(x, length(inc))
  }

  x <- first_of_isoweek(x)
  as_date_yw(x + 7L * inc)
}




increment.date_y <- function(x, inc){
  if (is_scalar(x) && length(inc) > 1){
    x <- rep(x, length(inc))
  }

  date_y(as.integer(x) + inc)
}
