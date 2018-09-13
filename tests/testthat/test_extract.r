context("extract")




test_that("test [[<-.date_xx functions", {
  x <- date_yq(2016, 1:2)
  x[[2]] <- c(20161)
  expect_identical(x, date_yq(2016, c(1, 1)))
  expect_error(x[[2]] <- c(20165), "valid")

  x <- date_ym(2016, 10:11)
  x[[2]] <- c(201610)
  expect_identical(x, date_ym(2016, c(10, 10)))
  expect_error(x[[2]] <- c(20165), "valid")

  x <- date_yw(2016, 10:11)
  x[[2]] <- c(201610)
  expect_identical(x, date_yw(2016, c(10, 10)))
  expect_error(x[[2]] <- c(20165), "valid")
})




test_that("test [<-.date_xx functions", {
  x <- date_yq(2016, 3:4)
  x[1:2] <- c(20161, 20161)
  expect_identical(x, date_yq(2016, c(1, 1)))
  expect_error(x[1:2] <- c(20165), "valid")
  expect_identical(x[1], date_yq(2016, 1))
  expect_identical(x[[1]], x[1])

  x <- date_ym(2016, 10:11)
  x[1:2] <- c(201610, 201610)
  expect_identical(x, date_ym(2016, c(10, 10)))
  expect_error(x[1:2] <- 201613, "valid")
  expect_identical(x[1], date_ym(2016, 10))
  expect_identical(x[[1]], x[1])

  x <- date_yw(2016, 10:11)
  x[1:2] <- c(201652, 201652)
  expect_identical(x, date_yw(2016, c(52, 52)))
  expect_error(x[2] <- 201654, "valid")
  expect_identical(x[1], date_yw(2016, 52))
  expect_identical(x[[1]], x[1])
})
