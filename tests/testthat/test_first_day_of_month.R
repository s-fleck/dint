context("first_day_of_month")


test_that("first_day_of_month works as expected", {

  # first day
  expect_identical(
    first_day_ym(2018, c(4, 12)),
    as.Date(c("2018-04-01", "2018-12-01"))
  )

  expect_identical(
    first_day_ym(2018, c(4, 12)),
    first_day_ym(as.Date(c("2018-04-13", "2018-12-01")))
  )

  # last day
    expect_identical(
      last_day_ym(c(2000, 2004, 2100), 2),
      as.Date(c("2000-02-29", "2004-02-29", "2100-02-28"))
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
