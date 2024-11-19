context("first_of")



# year --------------------------------------------------------------------

test_that("first_of_year works as expected", {
  # for Date
  expect_identical(
    first_of_year(as.Date("2018-01-03")),
    as.Date("2018-01-01")
  )
  expect_identical(
    last_of_year(as.Date("2018-01-03")),
    as.Date("2018-12-31")
  )

  # for date_xx
  expect_identical(
    first_of_year(date_yq(2018, 2)),
    as.Date("2018-01-01")
  )
  expect_identical(
    last_of_year(date_ym(2018, 1)),
    as.Date("2018-12-31")
  )

  # for numeric years
  expect_identical(
    first_of_year(2018),
    as.Date("2018-01-01")
  )
  expect_identical(
    last_of_year(2018),
    as.Date("2018-12-31")
  )
})



test_that("test first_of_month against lubridate", {
  if (!requireNamespace("lubridate", quietly = TRUE)){
    skip("test requires lubridate")
  }

  tdat <- c(make_date(2000, 1:12, 1), make_date(2002, 1:12, 1))

  expect_identical(
    first_of_year(tdat),
    lubridate::floor_date(tdat, "year")
  )

  expect_identical(
    last_of_year(tdat),
    lubridate::ceiling_date(tdat, "year") - 1
  )
})




# quarter -----------------------------------------------------------------
test_that("first_of_quarter works as expected", {
  expect_identical(
    first_of_quarter(make_date(2000, 1:12, 1)),
    c(
      rep(make_date(2000, 1, 1), 3),
      rep(make_date(2000, 4, 1), 3),
      rep(make_date(2000, 7, 1), 3),
      rep(make_date(2000, 10, 1), 3)
    )
  )

  expect_identical(
    last_of_quarter(make_date(2000, 1:12, 1)),
    c(
      rep(make_date(2000, 3, 31), 3),
      rep(make_date(2000, 6, 30), 3),
      rep(make_date(2000, 9, 30), 3),
      rep(make_date(2000, 12, 31), 3)
    )
  )

  expect_identical(
    unique(first_of_quarter(make_date(2000, 1:12, 1))),
    first_of_yq(2000, 1:4)
  )
  expect_identical(
    last_of_quarter(unique(last_of_quarter(make_date(2000, 1:12, 1)))),
    last_of_yq(2000, 1:4)
  )

  expect_identical(first_of_yq("2004-05-07"), as.Date("2004-04-01"))
  expect_identical(last_of_yq("2004-05-07"), as.Date("2004-06-30"))
})




test_that("test first_of_quarter against lubridate", {
  if (!requireNamespace("lubridate", quietly = TRUE)){
    skip("test requires lubridate")
  }

  tdat <- make_date(2000, 1:12, 1)

  expect_identical(
    first_of_quarter(tdat),
    lubridate::floor_date(tdat, "quarter")
  )
  expect_identical(
    last_of_quarter(tdat),
    lubridate::ceiling_date(tdat, "quarter") - 1
  )
})




# month -------------------------------------------------------------------

test_that("first_of_month works as expected", {
  # first day
  expect_identical(
    first_of_ym(2018, c(4, 12)),
    as.Date(c("2018-04-01", "2018-12-01"))
  )

  expect_identical(
    first_of_ym(2018, c(4, 12)),
    first_of_ym(as.Date(c("2018-04-13", "2018-12-01")))
  )

  # last day
    expect_identical(
      last_of_ym(c(2000, 2004, 2100), 2),
      as.Date(c("2000-02-29", "2004-02-29", "2100-02-28"))
    )

  expect_identical(
    last_of_month(as.Date("2018-05-13")),
    as.Date("2018-05-31")
  )

  expect_identical(
    last_of_ym(as.Date("2018-05-13")),
    last_of_ym(2018, 5)
  )
})




test_that("test first_of_month against lubridate", {
  if (!requireNamespace("lubridate", quietly = TRUE)){
    skip("test requires lubridate")
  }

  tdat <- c(make_date(2000, 1:12, 1), make_date(2002, 1:12, 1))

  expect_identical(
    first_of_month(tdat),
    lubridate::floor_date(tdat, "month")
  )

  expect_identical(
    last_of_month(tdat),
    lubridate::ceiling_date(tdat, "month") - 1
  )
})




# isoweek -----------------------------------------------------------------


test_that("first_of_isoweek works", {
  tdat <- seq(as.Date("2004-12-20"), as.Date("2020-01-06"), by = "7 days")

  expect_equal(
    first_of_isoweek(tdat),
    tdat
  )

  expect_equal(
    last_of_isoweek(tdat),
    tdat + 6L
  )

  expect_equal(
    seq(as.Date("2004-01-05"), as.Date("2004-12-20"), by = "7 days"),
    first_of_yw(2004, 2:52)
  )
  expect_equal(
    seq(as.Date("2004-01-11"), as.Date("2004-12-26"), by = "7 days"),
    last_of_yw(2004, 2:52)
  )

  expect_equal(first_of_yw("2004-01-07"), as.Date("2004-01-05"))
  expect_equal(last_of_yw("2004-01-07"), as.Date("2004-01-11"))
})




test_that("first_of_isoyear works", {
  eres <- data.frame(
    year = c(2018, 2017, 2016, 2005, 2004),
    from = as.Date(c("2018-01-01", "2017-01-02", "2016-01-04", "2005-01-03", "2003-12-29")),
    to   = as.Date(c("2018-12-30", "2017-12-31", "2017-01-01", "2006-01-01", "2005-01-02"))
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
    last_of_isoyear(as_date_y(eres$year)),
    eres$to
  )

  expect_identical(
    last_of_isoyear(date_yw(eres$year, 1L)),
    eres$to
  )
})




test_that("first_of_isoweek and first_of_isoyear are consistent", {
  eres <- data.frame(
    year = c(2018, 2017, 2016, 2005, 2004),
    from = as.Date(c("2018-01-01", "2017-01-02", "2016-01-04", "2005-01-03", "2003-12-29")),
    to   = as.Date(c("2018-12-30", "2017-12-31", "2017-01-01", "2006-01-01", "2005-01-02"))
  )

  expect_identical(
    first_of_isoyear(date_yw(eres$year, 1L)),
    first_of_isoweek(date_yw(eres$year, 1L))
  )

  expect_identical(
    first_of_isoweek(date_yw(eres$year, 1L)) + 6L,
    last_of_isoweek(date_yw(eres$year, 1L))
  )
})




test_that("first_of_iso* can handle anything coerciable to date", {
  expect_identical(
    first_of_isoweek("2018-09-07"),
    as.Date("2018-09-03")
  )

  expect_identical(
    last_of_isoweek("2018-09-07"),
    as.Date("2018-09-09")
  )

  expect_identical(
    first_of_isoyear("2016-09-07"),
    as.Date("2016-01-04")
  )

  expect_identical(
    last_of_isoyear("2016-09-07"),
    as.Date("2017-01-01")
  )
})
