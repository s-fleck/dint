context("accessors")


test_that("get_* returns integers", {
  d <- Sys.Date()
  ym <- date_ym(2018, 1)
  yq <- date_yq(2018, 1)
  y <-  date_y(2018)

  expect_true(is.integer(get_month(d)))
  expect_true(is.integer(get_month(ym)))
  expect_true(is.integer(get_month(yq)))

  expect_true(is.integer(get_quarter(d)))
  expect_true(is.integer(get_quarter(ym)))
  expect_true(is.integer(get_quarter(yq)))

  expect_true(is.integer(get_year(d)))
  expect_true(is.integer(get_year(ym)))
  expect_true(is.integer(get_year(yq)))

  expect_error(get_month(y))
  expect_error(get_quarter(y))
})



test_that("lubridate accessors work", {
  if (!requireNamespace("lubridate", quietly = TRUE)){
    skip("test requires lubridate")
  }
  ym <- date_ym(2018, 1)
  expect_identical(lubridate::year(ym), get_year(ym))
  expect_identical(lubridate::month(ym), get_month(ym))
  expect_identical(lubridate::quarter(ym), get_quarter(ym))
})
