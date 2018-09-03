context("first_day_of_week")


test_that("first_day_of_week works as expected", {

  tdat <- as.Date("2018-08-30")
  x <- date_yw(2018, 35)


  eres <- data.frame(
    year = c(2018, 2017, 2016, 2005, 2004),
    from = as.Date(c("2018-01-01", "2017-01-02", "2016-01-04", "2005-01-03", "2003-12-29"))
  )


  expect_identical(
    first_of_isoyear(as_date_y(eres$year)),
    eres$from
  )


  expect_identical(
    first_of_isoyear(date_yw(eres$year, 1L)),
    eres$from
  )


  expect_identical(
    first_of_isoyear(date_yw(eres$year, 1L)),
    first_of_isoweek(date_yw(eres$year, 1L))
  )


  expect_identical(
    first_of_isoweek(date_yw(eres$year, 1L)) + 7L,
    last_of_isoweek(date_yw(eres$year, 1L))
  )


})
