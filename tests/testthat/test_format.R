context("format")


all_tokens <- c("Y:%Y", "y:%y", "b:%b", "B:%B", "m:%m", "q:%q", "%%:%%", "W:%W")



test_that("tokenize_format works as expected", {
  fm <- "Y%Y-y%y-q%q-%%%yY"

  # normal cases
  expect_identical(
    tokenize_format(fm),
    c("Y", "%Y", "-y", "%y", "-q", "%q", "-", "%%", "%y", "Y")
  )

  fm <- "%Y-y%y-q%q-%%y"
  expect_identical(
    tokenize_format(fm),
    c("%Y", "-y", "%y", "-q", "%q", "-", "%%", "y")
  )

  # special cases
  expect_identical(tokenize_format("%y"), "%y")
  expect_identical(tokenize_format("y"), "y")
  expect_identical(tokenize_format(""), "")
  expect_identical(tokenize_format(character()), character())
})





test_that("format date_y works as expected", {
  td <- date_y(2010:2013)
  eres <- c("Y2010-y10-%10Y", "Y2011-y11-%11Y", "Y2012-y12-%12Y", "Y2013-y13-%13Y")

  # normal cases
  expect_identical(format(td, "Y%Y-y%y-%%%yY"), eres)
  expect_identical(format(td, "%Y-y%y-%%%y"), substr(eres, 2, nchar(eres) - 1))

  # special_cases
  expect_identical(format(td, ""), rep("", times = length(td)))
  date0 <- date_y(integer())
  expect_identical(format(date0), character())

  # violated preconditions
  expect_error(format(td, format = character()))
  expect_error(
    format(td, format = paste(all_tokens, collapse = " ")),
    "%b, %B, %m, %q, %W"
  )
})




test_that("format.date_yq: All Tokens Work", {
  fmt <- "y:%y Y:%Y q:%q m:%m b:%b B:%B %%:%%"
  q <- date_yq(2005, 1)
  expect_identical(format(q), "2005-Q1")
  expect_identical(
    format(q, fmt, month_names = month.name, month_abb = month.abb),
    "y:05 Y:2005 q:1 m:01 b:Jan B:January %:%"
  )

  # abort on illegal tokens
  fmt <- "%s %F %b"
  q <- date_yq(2005, 1)
  expect_error(format(q, format = paste(all_tokens, collapse = " ")), "%W")
})



test_that("format.date_yq: works for vectors", {
  td <- date_yq(2010:2019, 3)
  eres <- c(
    "Y2010-y10-q3-%%10Y", "Y2011-y11-q3-%%11Y", "Y2012-y12-q3-%%12Y",
    "Y2013-y13-q3-%%13Y", "Y2014-y14-q3-%%14Y", "Y2015-y15-q3-%%15Y",
    "Y2016-y16-q3-%%16Y", "Y2017-y17-q3-%%17Y", "Y2018-y18-q3-%%18Y",
    "Y2019-y19-q3-%%19Y"
  )

  # normal cases
  expect_identical(format(td, "Y%Y-y%y-q%q-%%%%%yY"), eres)
  expect_identical(format(td, "%Y-y%y-q%q-%%%%%y"), substr(eres, 2, nchar(eres) - 1))

  # special_cases
  expect_identical(format(td, ""), rep("", times = length(td)))
  date0 <- date_yq(integer(), integer())
  expect_identical(format(date0), character())

  # violated preconditions
  expect_error(format(td, format = character()))
})




test_that("format.date_ym: All Tokens Work", {
  fmt <- "y:%y Y:%Y q:%q m:%m b:%b B:%B %%:%%"
  m <- date_ym(2005, 11)
  expect_identical(format(m), "2005-M11")
  expect_identical(
    format(m, fmt, month_names = month.name, month_abb = month.abb),
    "y:05 Y:2005 q:4 m:11 b:Nov B:November %:%"
  )

  # abort on illegal tokens
  expect_error(format(m, format = paste(all_tokens, collapse = " ")), "%W")
})



test_that("format.date_yw: All Tokens Work", {
  fmt <- "y:%y Y:%Y W:%W %%:%%"
  m <- date_yw(2005, 11)
  expect_identical(format(m), "2005-W11")
  expect_identical(
    format(m, fmt, month_names = month.name, month_abb = month.abb),
    "y:05 Y:2005 W:11 %:%"
  )

  # abort on illegal tokens
  expect_error(format(m, format = paste(all_tokens, collapse = " ")), "%b, %B, %m, %q")
})




test_that("format.date_ym: month names are formatted correclty", {
  td <- date_ym(2018, c(1L, 10L, 3L, 6L, 4L, 5L, 7L, 12L, 2L, 9L, 8L, 11L))
  fm <- "%Y-M%m: %B,%b"

  tres <- format(
    td,
    format = fm,
    month_names = month.name,
    month_abb = month.abb
  )

  eres <- c(
    "2018-M01: January,Jan", "2018-M10: October,Oct", "2018-M03: March,Mar",
    "2018-M06: June,Jun", "2018-M04: April,Apr", "2018-M05: May,May",
    "2018-M07: July,Jul", "2018-M12: December,Dec", "2018-M02: February,Feb",
    "2018-M09: September,Sep", "2018-M08: August,Aug", "2018-M11: November,Nov"
  )

  expect_identical(eres, tres)
})




test_that("default formats are ISO formats", {
  expect_identical(format(date_ym(2018, 5)),  "2018-M05")
  expect_identical(format(date_yq(2018, 1)),  "2018-Q1")
  expect_identical(format(date_yw(2018, 51)), "2018-W51")
})




test_that("format_y* defaults to iso", {
  d <- as.Date("2018-12-10")
  expect_identical(format_yq(d), "2018-Q4")
  expect_identical(format_ym(d), "2018-M12")
  expect_identical(format_yw(d), "2018-W50")

  d <- as.Date("2018-01-1")
  expect_identical(format_yq_iso(d), format_yq(d))
  expect_identical(format_yq_short(d), "2018.1")
  expect_identical(format_yq_shorter(d), "18.1")

  expect_identical(format_ym_iso(d), format_ym(d))
  expect_identical(format_ym_short(d), "2018.01")
  expect_identical(format_ym_shorter(d), "18.01")

  expect_identical(format_yw_iso(d), format_yw(d))
  expect_identical(format_yw_short(d), "2018.01")
  expect_identical(format_yw_shorter(d), "18.01")
})

