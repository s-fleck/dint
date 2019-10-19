context("parser")


test_that("parser works as expected", {

  x <- c("dfw2018q2", "bl2017qasdg4sadgfas", "201712", "blubb", "2017-2016", NA)

  expect_warning(
    r <- as.integer(yq(x)),
    "3 failed"
  )

  expect_identical(
    r,
    c(20182L, 20174L, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
  )


  x <- c("d2fw2018", "b4l2017", "122017", "blubb", "2017-2016", NA)

  expect_warning(
    r <- as.integer(qy(x)),
    "3 failed"
  )

  expect_identical(
    r,
    c(20182L, 20174L, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
  )

})
