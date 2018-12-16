context("date_y")


test_that("date_y works as expected", {
  expect_identical(as.Date(date_y(2018)), as.Date(c("2018-01-01")))
})
