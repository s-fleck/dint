library(dint)
library(bench)
library(ggplot2)
library(stringi)


format_with_paste <- dint:::format_date_xx
tokenize_format <-  dint:::tokenize_format

# alternative functions ---------------------------------------------------

format_with_matrix <- function(x, format){
  tokens <- tokenize_format(format)
  len <- length(tokens)

  res <- matrix(nrow = length(x), ncol = len, data = NA_character_)

  year    <- get_year(x)
  month   <- get_month(x)
  quarter <- get_quarter(x)

  for(i in seq_len(len)){
    if (tokens[[i]] == "%Y")
      res[, i] <- year
    else if (tokens[[i]] == "%y")
      res[, i] <- year %% 100
    else if (tokens[[i]] == "%q")
      res[, i] <- quarter
    else if (tokens[[i]] == "%m")
      res[, i] <- month
    else
      res[, i] <- tokens[[i]]
  }

  apply(res, 1, paste, collapse = "")
}



format_with_gsub <- function(
  x,
  format,
  preset = NULL,
  ...
){
  gsub.vec <- function(pattern, replacement, x, ...) {
    y <- x
    for(i in seq_along(x)) {
      y[i] <- gsub(pattern, replacement[i], x[i], ..., fixed = TRUE)
    }
    y
  }


  res <- gsub.vec("%q", get_quarter(x), rep.int(format, length(x)))
  res <- gsub.vec("%Y", get_year(x), res)
  res <- gsub.vec("%y", get_year(x) %% 100, res)
  res
}



format_with_stringi <- function(
  x,
  format,
  preset = NULL,
  ...
){
  res <- stringi::stri_replace_all_fixed(format, "%q", get_quarter(x))
  res <- stringi::stri_replace_all_fixed(res, "%y", get_year(x) %% 100)
  res <- stringi::stri_replace_all_fixed(res, "%Y", get_year(x))
  res
}



# benchmark ---------------------------------------------------------------

tdat <- as_date_yq(rep(date_yq(1000:2000, 1), 100))


res <- mark(
  matrix = format_with_matrix(tdat, format = "Y%Y-y%y-q%q"),
  gsub = format_with_gsub(tdat, format = "Y%Y-y%y-q%q"),
  stringi = format_with_stringi(tdat, format = "Y%Y-y%y-q%q"),
  paste = format_with_paste(tdat, format = "Y%Y-y%y-q%q"),
  min_iterations = 10
)


print(res)
plot(res)
