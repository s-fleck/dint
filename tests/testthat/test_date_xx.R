context("date_xx")




test_that("common generics have methods for date_xx", {
  q <- date_yq(2014, c(1, 1, 1, 1:4))
  m <- date_ym(2014, c(1, 1, 1, 1:12))
  w <- date_yw(2014, c(1, 1, 1, 1:52))
  y <- date_y(c(2014, 2014, 2015, 2015, 2015))

  # unique
  expect_identical(unique(q), date_yq(2014, 1:4))
  expect_identical(unique(m), date_ym(2014, 1:12))
  expect_identical(unique(w), date_yw(2014, 1:52))
  expect_identical(unique(y), date_y(2014:2015))
})




test_that("make_date_xx works", {
  expect_identical(make_date_xx(2012), date_y(2012))
  expect_identical(make_date_xx(2012, 4), date_yq(2012, 4))
  expect_identical(make_date_xx(2012, m = 4), date_ym(2012, 4))
})




test_that("print date_xx returns input", {
  expect_output(expect_identical(print(make_date_xx(2012)), make_date_xx(2012)))
})




test_that("date_xx coercion to POSIXt works", {
  expect_identical(as.POSIXlt(make_date_xx(2012)), as.POSIXlt(as.Date("2012-01-01")))
  expect_identical(as.POSIXct(make_date_xx(2012)), as.POSIXct(as.Date("2012-01-01")))
})




test_that("Sys.date_** return the appropriate types", {
  is_date_yq(Sys.date_yq())
  is_date_ym(Sys.date_ym())
  is_date_yw(Sys.date_yw())
})
