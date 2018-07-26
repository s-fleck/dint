context("first_day_of_quarter")


test_that("first_day_of_quarter works as expected", {

  expect_identical(
    first_day_of_quarter(make_date(2000, 1:12, 1)),
    c(
      rep(make_date(2000, 1, 1), 3),
      rep(make_date(2000, 4, 1), 3),
      rep(make_date(2000, 7, 1), 3),
      rep(make_date(2000, 10, 1), 3)
    )
  )


  expect_identical(
    last_day_of_quarter(make_date(2000, 1:12, 1)),
    c(
      rep(make_date(2000, 3, 31), 3),
      rep(make_date(2000, 6, 30), 3),
      rep(make_date(2000, 9, 30), 3),
      rep(make_date(2000, 12, 31), 3)
    )
  )

})




test_that("test first_day_of_quarter against lubridate", {

  if (!requireNamespace("lubridate", quietly = TRUE)){
    skip("test requires lubridate")
  }

  tdat <- make_date(2000, 1:12, 1)

  expect_identical(
    first_day_of_quarter(tdat),
    lubridate::floor_date(tdat, "quarter")
  )

  expect_identical(
    last_day_of_quarter(tdat),
    lubridate::ceiling_date(tdat, "quarter") - 1
  )
})

