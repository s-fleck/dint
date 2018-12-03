#' @keywords internal
"_PACKAGE"



.onLoad <- function(...) {
  dyn_register_s3_method("lubridate", "year", "date_xx")
  dyn_register_s3_method("lubridate", "month", "date_xx")
  dyn_register_s3_method("zoo", "as.yearqtr", "date_yq")
  dyn_register_s3_method("zoo", "as.yearmon", "date_ym")

  # +- yq --------------------------------------------------------------------
  if (requireNamespace("scales", quietly = TRUE)){
    assign(
      "date_yq_trans",
      envir = parent.env(environment()),
      scales::trans_new(
        name = "date_yq",
        transform = as_yearqtr.date_yq,
        inverse   = function(x){
          x <- round_frac(as.numeric(x), 4)
          as_date_yq.yearqtr(x)
        },
        breaks = date_yq_breaks(),
        format = function(x){
          if (all(get_quarter(x) == 1L | is.na(x))){
            as.character(get_year(x))
          } else {
            format_yq_short(x)
          }
        }
      )
    )

  # +- ym --------------------------------------------------------------------
    assign(
      "date_ym_trans",
      envir = parent.env(environment()),
        scales::trans_new(
        name = "date_ym",
        transform = as_yearmon.date_ym,
        inverse   = function(x){
          x <- round_frac(as.numeric(x), 12)
          as_date_ym.yearmon(x)
        },
        breaks = date_ym_breaks(),
        format = function(x){
          if (all(get_month(x) == 1L | is.na(x))){
            as.character(get_year(x))
          } else {
            format_ym_short(x)
          }
        }
      )
    )


  # +- yw --------------------------------------------------------------------
    assign(
      "date_yw_trans",
      envir = parent.env(environment()),
      scales::trans_new(
        name = "date_yw",
        transform = function(x) {
          as.numeric(as.Date(x))
        },
        inverse = function(x) {
          origin <- structure(0, class = c("POSIXct", "POSIXt"), tzone = "UTC")
          as_date_yw(as.Date.numeric(x, origin = origin))
        } ,
        breaks  = date_yw_breaks(),
        format = function(x){
          if (all(get_isoweek(x) == 1L | is.na(x))){
            as.character(get_year(x))
          } else {
            format_yw_short(x)
          }
        }
      )
    )
  }

  invisible()
}
