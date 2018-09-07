context("arithmetic")



# seq ---------------------------------------------------------------------

test_that("seq.date_yw works", {
  x <- seq(date_yw(2017, 1), date_yw(2019, 1))

  expect_identical(
    as.integer(x),
    c(201701:201752, 201801:201853, 201901)
  )
})




test_that("seq.date_ym works", {
  x <- seq(date_ym(2017, 1), date_ym(2019, 1))

  expect_identical(
    as.integer(x),
    as.integer(c(201701:201712, 201801:201812, 201901))
  )
})




test_that("seq.date_ym works", {
  x <- seq(date_yq(2017, 1), date_yq(2019, 1))

  expect_identical(
    as.integer(x),
    as.integer(c(20171:20174, 20181:20184, 20191))
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
