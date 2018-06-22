context("first_day_of_month")


test_that("first_day_of_month works as expected", {

  expect_identical(
    first_day_ym(2018, 4),
    as.Date("2018-04-01")
  )

  expect_identical(
    first_day_ym(2018, 4),
    first_day_ym(as.Date("2018-04-13"))
  )

  expect_identical(
    first_day_ym(2018, 4),
    first_day_of_month(as.Date("2018-04-30"))
  )

  expect_identical(
    last_day_of_month(as.Date("2018-05-13")),
    as.Date("2018-05-31")
  )
  expect_identical(
    last_day_ym(as.Date("2018-05-13")),
    last_day_ym(2018, 5)
  )







})
