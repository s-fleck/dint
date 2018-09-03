context("accessors")


test_that("get_* returns integers", {
  d <- Sys.Date()
  ym <- date_ym(2018, 1)
  yq <- date_yq(2018, 1)


  expect_true(is.integer(get_month(d)))
  expect_true(is.integer(get_month(ym)))
  expect_true(is.integer(get_month(yq)))

  expect_true(is.integer(get_quarter(d)))
  expect_true(is.integer(get_quarter(ym)))
  expect_true(is.integer(get_quarter(yq)))

  expect_true(is.integer(get_year(d)))
  expect_true(is.integer(get_year(ym)))
  expect_true(is.integer(get_year(yq)))

})




test_that("get_week behaves like lubridate::isoweek", {

  x <- as.Date(c("2018-01-23", "2009-12-31", "2009-01-01", "2010-01-01", "2005-01-01"))

  expect_identical(
    lubridate::isoweek(x),
    get_isoweek(x)
  )

  expect_identical(
    lubridate::isoweek(as_date_yq(x)),
    get_isoweek(as_date_yq(x))
  )

  expect_identical(
    lubridate::isoweek(as_date_ym(x)),
    get_isoweek(as_date_ym(x))
  )

})
