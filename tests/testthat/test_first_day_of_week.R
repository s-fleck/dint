context("first_day_of_week")


test_that("first_day_of_week works as expected", {

  tdat <- as.Date("2018-08-30")

  x <- date_yw(2018, 35)

  first_day_of_week(x)



})
