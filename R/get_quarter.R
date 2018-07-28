#' @rdname getters
#' @export
#' @examples
#' x <- date_yq(2016, 2)
#' get_quarter(x)
#' \dontrun{
#' library(lubridate)
#' quarter(x)
#' }
#'
get_quarter <- function(x){
  UseMethod("get_quarter")
}




#' @export
get_quarter.default <- function(x){
  as.integer(ceiling(get_month(x) / 3))
}




#' @export
get_quarter.date_y <- function(x){
  stop("Not supported for date_y objects")
}




#' @export
get_quarter.date_yq <- function(x){
  as.integer(x) %% 10
}
