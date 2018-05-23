x <- data.frame(
  date = c(Sys.Date(), Sys.Date(), Sys.Date())
)

x$ym <- as_date_ym(x$date)
x$yq <- as_date_yq(x$date)


x


x$ym
x$yq
