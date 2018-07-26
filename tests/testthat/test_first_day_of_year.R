context("first_day_of_year")


test_that("first_day_of_year works as expected", {

  x    <- as.Date("2018-01-03")

  expect_identical(
    first_day_of_year(x),
    as.Date("2018-01-01")
  )

  expect_identical(
    last_day_of_year(x),
    as.Date("2018-12-31")
  )


})
