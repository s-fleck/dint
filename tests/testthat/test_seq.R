context("seq")

# 2007 starts on jan 1st
# 2008 is a leaop year
# 2009 has 53 weeks
# 2010 is a gernic year without special properties

test_that("seq.date_yw works", {
  x <- seq(date_yw(2007, 1), date_yw(2010, 52))
  expect_identical(
    as.integer(x),
    c(200701:200752, 200801:200852, 200901:200953, 201001:201052)
  )

  tdat  <- date_yw(c(2002, 2020), c(50, 2))
  tdat2 <- date_yw(c(2002, 2020), c(50, 50))

  expect_identical(
    seq(tdat[1], tdat[2]),
    as_date_yw(seq(as.Date("2002-12-10"), as.Date("2020-01-07"), by = "7 days"))
  )

  expect_identical(
    as.integer(seq(tdat[1], tdat[2], by = "isoyear")),
    (2002:2019) * 100L + 50L
  )

  expect_identical(
    as.integer(seq(tdat2[1], tdat2[2], by = "isoyear")),
    (2002:2020) * 100L + 50L
  )

  expect_identical(
    as.Date(seq(tdat2[1], tdat2[2], by = "2 years")),
    seq(as.Date(tdat[1]), as.Date(tdat[2]), by = "730 days")
  )

})




test_that("seq.date_ym works", {
  x <- seq(date_ym(2007, 1), date_ym(2009, 1))

  expect_identical(
    as.integer(x),
    as.integer(c(200701:200712, 200801:200812, 200901))
  )
})




test_that("seq.date_yq works", {
  x <- seq(date_yq(2007, 1), date_yq(2009, 1))

  expect_identical(
    as.integer(x),
    as.integer(c(20071:20074, 20081:20084, 20091))
  )
})




test_that("parse_seq_by works", {

  expect_identical(parse_seq_by(1L), 1L)
  expect_identical(parse_seq_by(1), 1L)
  expect_identical(
    parse_seq_by("5 years"),
    structure(5L, unit = "year")
  )
  expect_identical(
    parse_seq_by("5 years"),
    structure(5L, unit = "year")
  )
  expect_identical(
    parse_seq_by("year"),
    structure(1L, unit = "year")
  )




  # faulty inputs
  expect_error(parse_seq_by(1.2), "integer")
  expect_error(parse_seq_by(1:3), "scalar")
  expect_error(parse_seq_by("1.5 years"), "integer")
  expect_error(parse_seq_by("1"), "not a valid unit")
  expect_error(parse_seq_by("5 blubbs"), "not a valid unit")





})


