context("format")



test_that("format date_yq works as expected", {
  td <- date_yq(2010:2019, 3)
  eres <- c(
    "Y2010-y10-q3-%10Y", "Y2011-y11-q3-%11Y", "Y2012-y12-q3-%12Y",
    "Y2013-y13-q3-%13Y", "Y2014-y14-q3-%14Y", "Y2015-y15-q3-%15Y",
    "Y2016-y16-q3-%16Y", "Y2017-y17-q3-%17Y", "Y2018-y18-q3-%18Y",
    "Y2019-y19-q3-%19Y"
  )

  # normal cases
  expect_identical(format(td, "Y%Y-y%y-q%q-%%yY"), eres)
  expect_identical(format(td, "%Y-y%y-q%q-%%y"), substr(eres, 2, nchar(eres) - 1))

  # special_cases
  expect_identical(format(td, ""), rep("", times = length(td)))
  date0 <- date_yq(integer(), integer())
  expect_identical(format(date0), character())

  # violated preconditions
  expect_error(format(td, format = character()))
})



test_that("tokenize_format works as expected", {
  fm <- "Y%Y-y%y-q%q-%%yY"

  # normal cases
  expect_identical(
    tokenize_format(fm),
    c("Y", "%Y", "-y", "%y", "-q", "%q", "-%", "%y", "Y")
  )
  fm <- "%Y-y%y-q%q-%%y"
  expect_identical(
    tokenize_format(fm),
    c("%Y", "-y", "%y", "-q", "%q", "-%", "%y")
  )

  # special cases
  expect_identical(tokenize_format("%y"), "%y")
  expect_identical(tokenize_format("y"), "y")
  expect_identical(tokenize_format(""), "")
  expect_identical(tokenize_format(character()), character())
})



test_that("month names are formatted correclty", {
  td <- date_ym(2018, c(1L, 10L, 3L, 6L, 4L, 5L, 7L, 12L, 2L, 9L, 8L, 11L))
  fm <- "%Y-M%m: %B,%b"

  tres <- format.date_xx(
    td,
    format = fm,
    month_names = month.name,
    month_abb = month.abb
  )

  eres <- c(
    "2018-M1: January,Jan", "2018-M10: October,Oct", "2018-M3: March,Mar",
    "2018-M6: June,Jun", "2018-M4: April,Apr", "2018-M5: May,May",
    "2018-M7: July,Jul", "2018-M12: December,Dec", "2018-M2: February,Feb",
    "2018-M9: September,Sep", "2018-M8: August,Aug", "2018-M11: November,Nov"
  )

  expect_identical(eres, tres)
})



