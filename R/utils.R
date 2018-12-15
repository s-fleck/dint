# imitatie behaviour of lubridate
tz <- function(x){
  tzone <- attr(x, "tzone")[[1]]
  if (is.character(tzone) && nzchar(tzone)){
    tzone
  } else {
    "UTC"
  }
}




# use the more efficient make_date function from lubridate if it is available
make_date <- function(y, m, d){
  if (requireNamespace("lubridate", quietly = TRUE)){
    lubridate::make_date(y, m, d)
  } else {
    as.Date(ISOdate(y, m, d))
  }
}




substr_right <- function(x, n){
  nc <- nchar(x)
  substr(x, nc - n + 1L, nc)
}




dyn_register_s3_method <- function(
  pkg,
  generic,
  class,
  fun = NULL
){
  stopifnot(
    is_scalar_character(pkg),
    is_scalar_character(generic),
    is_scalar_character(class)
  )

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}




ifelse_simple <- function(x, true, false){
  assert(is.logical(x))
  assert(is_equal_length(x, true, false))
  assert(identical(class(true), class(false)))
  false[x] <- true[x]
  false
}




is_date_yq_integerish <- function(x){
  all(unclass(x) %% 10L %in% 1:4 | is.na(x))
}




is_date_ym_integerish <- function(x){
  all(unclass(x) %% 100L %in% 1:12 | is.na(x))
}




is_date_yw_integerish <- function(x){
  all(unclass(x) %% 100L %in% 1:53 | is.na(x))
}




is_scalar_date_yq_integerish <- function(x){
  identical(length(x), 1L) && unclass(x) %% 10L %in% 1:4 | is.na(x)
}




is_scalar_date_ym_integerish <- function(x){
  identical(length(x), 1L) && unclass(x) %% 100L %in% 1:12 | is.na(x)
}




is_scalar_date_yw_integerish <- function(x){
  identical(length(x), 1L) && unclass(x) %% 100L %in% 1:53 | is.na(x)
}




which_date_xx <- function(
  x
){
  dates <- c("date_yq", "date_ym", "date_yw", "date_y")
  sel <- dates %in% class(x)
  assert(
    sum(sel) == 1L,
    "'x' is not a valid <date_xx> vector: check it's class attribute."
  )
  dates[sel]
}




random_date_xx <- function(n, mode, replace = TRUE, years = 2010:2020){

  if (identical(mode, "date_yq")){
    x <-  seq(date_yq(min(years), 1), date_yq(max(years), 4))
  } else if (identical(mode, "date_ym")){
    x <-  seq(date_ym(min(years), 1), date_ym(max(years), 12))
  } else if (identical(mode, "date_yw")){
    x <- seq(date_yw(min(years), 1), as_date_yw(last_of_isoyear(max(years))))
  }

  sample(x, n, replace = replace)
}
