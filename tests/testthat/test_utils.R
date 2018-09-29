context("utils")


test_that("utils works as expected", {

  q <- date_yq(2018, 1)
  m <- date_ym(2018, 1)
  w <- date_yw(2018, 1)
  y <- date_y(2018)
  a <- structure("test", class = c("date_yq", "blahblah", "date_xx"))
  b <- structure("test", class = c("date_yq", "date_ym", "date_xx"))

  expect_identical(which_date_xx(q), "date_yq")
  expect_identical(which_date_xx(m), "date_ym")
  expect_identical(which_date_xx(w), "date_yw")
  expect_identical(which_date_xx(y), "date_y")

  expect_identical(which_date_xx(a), "date_yq")
  expect_error(which_date_xx(b))
})
