context("date_yq")


test_that("date_yq ctor works as expected", {
  expect_silent(tr1 <- date_yq(2015, 2))
  expect_true(is.na(date_yq(2015, NA)))
  expect_true(is.na(date_yq(NA, 4)))
  expect_true(is.na(date_yq(NA, NA)))

  expect_identical(as.Date(tr1), as.Date('2015-04-01'))
  expect_identical(as.Date(tr1), as.Date('2015-04-01'))

  tr2 <- as_date_yq(20152)
  expect_identical(tr1, tr2)

  tr3 <- as_date_yq(as.Date('2015-05-02'))
  expect_identical(tr1, tr3)

  expect_identical(
    date_yq(0, 1),
    structure(1L, class = c("date_yq", "date_xx", "integer"))
  )

  expect_identical(
    date_yq(-1, 1),
    structure(-11L, class = c("date_yq", "date_xx", "integer"))
  )

  expect_error(date_yq(0, -1))
})


test_that("as_date_yq works", {
  tdat <- c(-104, -11, 1, 12, 103, 1004, 20001, 212342)

  expect_error(as_date_yq(-1))
  expect_silent(
    tres <- as_date_yq(tdat)
  )

  expect_identical(as.integer(tres), as.integer(tdat))
})



test_that("format.date_yq works as expected", {
  tdat <- as_date_yq(c(-104, -11, 1, 12, 103, 1004, 20001, 212342))

  expect_identical(
    format(tdat),
    c("-10-Q4", "-1-Q1", "0-Q1", "1-Q2", "10-Q3", "100-Q4", "2000-Q1",
      "21234-Q2")
  )

  expect_identical(
    format(tdat, "short"),
    c("-10.4", "-1.1", "0.1", "1.2", "10.3", "100.4", "2000.1", "21234.2")
  )

  expect_identical(
    format(tdat, "shorter"),
    c("-10.4", "-1.1", "0.1", "1.2", "10.3", "00.4", "00.1", "34.2")
  )
})


test_that("as_date_yq arithmetics works", {
  #* @testing increment.date_yq
  tdat <- (as_date_yq(c(-11, -12, -13, -14, 1, 2, 3, 4)))

  expect_identical(
    tdat + 1,
    as_date_yq(c(-12, -13, -14, 1, 2, 3, 4, 11))
  )

  expect_identical(
    tdat + 5,
    as_date_yq(c(2, 3, 4, 11, 12, 13, 14, 21))
  )

  expect_identical(
    tdat - 1,
    as_date_yq(c(-24, -11, -12, -13, -14, 1, 2, 3))
  )

  expect_identical(
    tdat - 5,
    as_date_yq(c(-34, -21, -22, -23, -24, -11, -12, -13))
  )

})



