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



test_that("test first_day_of_month against lubridate", {

  if (!requireNamespace("lubridate", quietly = TRUE)){
    skip("test requires lubridate")
  }

  tdat <- c(make_date(2000, 1:12, 1), make_date(2002, 1:12, 1))

  expect_identical(
    first_day_of_year(tdat),
    lubridate::floor_date(tdat, "year")
  )

  expect_identical(
    last_day_of_year(tdat),
    lubridate::ceiling_date(tdat, "year") - 1
  )
})
