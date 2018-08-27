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
