context("parser")


test_that("yq/qy work as expected", {

  x <- c("dfw2018q2", "bl2017qasdg4sadgfas", "201712", "blubb", "2017-2016", NA)
  expect_warning(r <- as.integer(yq(x)), "3 failed")

  expect_identical(
    r,
    c(20182L, 20174L, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
  )


  x <- c("d2fw2018", "b4l2017", "122017", "blubb", "2017-2016", NA)

  expect_warning(r <- as.integer(qy(x)), "3 failed")

  expect_identical(
    r,
    c(20182L, 20174L, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
  )

  expect_identical(qy("stpQ42015"))



})





test_that("ym/my work as expected", {

  x <- c("dfw2018m02", "bl2017qasdg12sadgfas", "2017Q4", "blubb", "2017-2016", NA, "gvship_ais_2019-M10.fst")

  expect_warning(
    r <- as.integer(ym(x)),
    "3 failed"
  )

  expect_identical(
    r,
    c(201802L, 201712L, NA_integer_, NA_integer_, NA_integer_, NA_integer_, 201910L)
  )


  x <- c("d02fw2018", "b122017", "42017", "blubb", "2017-2016", NA)

  expect_warning(
    r <- as.integer(my(x)),
    "3 failed"
  )

  expect_identical(
    r,
    c(201802L, 201712L, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
  )

})
