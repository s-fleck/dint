assert_lubridate <- function(x){
  if (!require_lubridate())
    stop(paste0(
      "This function requires the package 'lubridate'.",
      "You can install it via install.packages('lubridate')"
    ))
}




require_lubridate <- function(x){
  requireNamespace("lubridate", quietly = TRUE)
}




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
  if (require_lubridate()){
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




deprecate <- function(
  fun,
  new = deparse(substitute(fun))
){

  function(...){
    .Deprecated(new)
    fun(...)
  }

}




is_scalar_integer <- function(x){
  is.integer(x) && is_scalar(x)
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

