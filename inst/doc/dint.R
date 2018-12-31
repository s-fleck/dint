## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(dint)

## ------------------------------------------------------------------------

# date_* Objects can be created using explicit constructors...
date_yq(2015, 1)
date_ym(c(2015, 2016), c(1, 2))
date_yw(c(2008, 2009), 1)

# ...or through coercion of dates or integers
as_date_yq(Sys.Date())
as_date_yq(20141)   # the last digit is interpreted as quarter
as_date_ym(201412)  # the last two digits are interpreted as month
as_date_yw("2018-01-01")  # anything else that can be parsed by as.Date() works


## ------------------------------------------------------------------------

# You can coerce dates to any date_xx object with as_date_**
d <- as.Date("2018-05-12")
as_date_yq(d)
as_date_ym(d)
as_date_yw(d)
as_date_y(d)


# Conversely, you can convert date_xx objects back to the various R Date formats
q <- date_yq(2015, 1)
as.Date(q)
as.POSIXlt(q)


# as.POSIXct creates datetimes in UTC/GMT, so the result might not always be as
# expected, depending on your local timezone.
as.POSIXct(q)
as.POSIXct(q, tz = "GMT")
print(as.POSIXct(q), tz = "GMT")
print(as.POSIXct(q), tz = "CET")


## ------------------------------------------------------------------------
# Quarters
q <- date_yq(2014, 4)
q
q + 1
seq(q -2, q + 2)


# Months
m <- date_ym(2014, 12)
m
m + 1
seq(m -2, m + 2)


## ------------------------------------------------------------------------
q <- date_yq(2014, 4)
get_year(q)
get_quarter(q)
get_month(q) # defaults to first month of quarter


m <- date_ym(2014, 12)
get_year(m)
get_quarter(m)
get_month(m)

## ------------------------------------------------------------------------
stopifnot(requireNamespace("lubridate"))

lubridate::year(q)
lubridate::quarter(q)
lubridate::month(q)


## ------------------------------------------------------------------------
q <- date_yq(2015, 1)
first_of_quarter(q)  # the same as as.Date(q), but more explicit
last_of_quarter(q)  # the same as as.Date(q), but more explicit


# These functions work with normal dates
d <- as.Date("2018-05-12")
first_of_year(d)
last_of_year(d)
first_of_quarter(d)
last_of_quarter(d)
first_of_month(d)
last_of_month(d)
first_of_isoweek(d)
last_of_isoweek(d)

# Alternativeley you can also use these:
first_of_yq(2012, 2)
last_of_ym(2012, 2)


## ------------------------------------------------------------------------
q <- date_yq(2014, 4)
format(q, "%Y-Q%q")  # iso/default
format(q, "%Y.%q")
format(q, "%y.%q")


m <- date_ym(2014, 12)
format(m, "%Y-M%m")  # iso/default

w <- date_yw(2014, 1)
format(w, "%Y-W%W")  # iso/default


# You can use these for coercion and formatting in one step
format_yq(Sys.Date())
format_ym(Sys.Date())
format_yw(Sys.Date())

## ---- collapse=TRUE, fig.show='hold'-------------------------------------
library(ggplot2)

x <- data.frame(
  time  = seq(as.Date("2016-01-01"), as.Date("2016-08-08"), by = "day")
)

x$value <- rnorm(nrow(x))

p <- ggplot(
  x,
  aes(
    x = time, 
    y = value)
  ) + geom_point()


p + ggtitle("iso") + ggtitle("default")
p + scale_x_date(labels = format_yq_iso) + ggtitle("date_yq_iso")
p + scale_x_date(labels = format_ym_short) + ggtitle("date_ym_short")
p + scale_x_date(labels = format_yw_shorter) + ggtitle("date_yw_shorter")


