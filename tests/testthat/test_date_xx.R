context("date_xx")


test_that("date_xx standard generics works as expected", {

  q <- date_yq(2014, c(1, 1, 1, 1:4))
  m <- date_ym(2014, c(1, 1, 1, 1:12))
  w <- date_yw(2014, c(1, 1, 1, 1:52))
  y <- date_y(c(2014, 2014, 2015, 2015, 2015))

  expect_identical(unique(q), date_yq(2014, 1:4))
  expect_identical(unique(m), date_ym(2014, 1:12))
  expect_identical(unique(w), date_yw(2014, 1:52))
  expect_identical(unique(y), date_y(2014:2015))
})
