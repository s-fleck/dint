#' Get quarter of a date_xx.
#'
#' `get_quarter()` returns the quarter of a [date_xx] object.
#' [lubridate::quarter()] also works, because \pkg{hammr} exports a method for
#' [lubridate::month()]. This will have more overhead than using
#' `get_quarter()`, but should generally be prefered for consistency in
#' function naming across packages.
#'
#' @inheritParams lubridate::year
#' @seealso [lubridate::quarter()]
#' @aliases quarter
#' @family yq getters
#' @export
#'
#' @examples
#'
#' x <- date_yq(2016, 2)
#' get_quarter(x)
#'
#' \dontrun{
#' library(lubridate)
#' quarter(x)
#' }
#'
#'
get_quarter <- function(x){
  UseMethod("get_quarter")
}




#' @export
get_quarter.date_y <- function(x){
  stop("Not supported for date_y objects")
}




#' @export
get_quarter.date_ym <- function(x){
  as.integer(ceiling(get_month(x) / 3))
}




#' @export
get_quarter.date_yq <- function(x){
  as.integer(x) %% 10
}
