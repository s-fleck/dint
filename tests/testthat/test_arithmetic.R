context("arithmetic")




# seq ---------------------------------------------------------------------

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

  expect_identical(
    as_date_yw(seq(as.Date("2004-12-20"), as.Date("2020-01-06"), by = "7 days")),
    seq(date_yw(2004, 52), date_yw(2020, 2))
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




# y+ ----------------------------------------------------------------------

test_that("y+.date_ym works", {
  expect_identical(
    date_ym(2017, 1) %y+% 1,
    date_ym(2018, 1)
  )

  expect_identical(
    date_ym(2017, 1) %y-% 1,
    date_ym(2016, 1)
  )
})




test_that("y+.date_yq arithmetic works", {
  expect_identical(
    date_yq(2017, 1) %y+% 1,
    date_yq(2018, 1)
  )

  expect_identical(
    date_yq(2017, 1) %y-% 1,
    date_yq(2016, 1)
  )
})
