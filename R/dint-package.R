#' @keywords internal
"_PACKAGE"



.onLoad <- function(...) {
  dyn_register_s3_method("lubridate", "year", "date_xx")
  dyn_register_s3_method("lubridate", "month", "date_xx")
  dyn_register_s3_method("zoo", "as.yearqtr", "date_yq")
  invisible()
}
