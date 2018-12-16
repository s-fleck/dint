context("scale_date_xx")


test_that("scale_date_xx works as expected", {
  if (!requireNamespace("ggplot2", quietly = TRUE)){
    skip("test requires ggplot2")
  }

  # Force all branches of the date_**_breaks and ensure no exception is raised
  # < 6
  tdf <- data.frame(
    yq = date_yq(2000, 1:4),
    ym = date_ym(2000, 4:7),
    yw = date_yw(2000, 21:24),
    vals = runif(4)
  )
  expect_silent(print(ggplot2::ggplot(tdf, ggplot2::aes(x = yq, y = vals))))
  expect_silent(print(ggplot2::ggplot(tdf, ggplot2::aes(x = ym, y = vals))))
  expect_silent(print(ggplot2::ggplot(tdf, ggplot2::aes(x = yw, y = vals))))

  # > 6
  tdf <- data.frame(
    yq = date_yq(c(rep(1990, 4), rep(1991, 4)), c(1:4, 1:4)),
    ym = date_ym(1990, 1:8),
    yw = date_yw(1990, 1:8),
    vals = runif(8)
  )
  expect_silent(print(ggplot2::ggplot(tdf, ggplot2::aes(y = yq, x = vals))))
  expect_silent(print(ggplot2::ggplot(tdf, ggplot2::aes(y = ym, x = vals))))
  expect_silent(print(ggplot2::ggplot(tdf, ggplot2::aes(y = yw, x = vals))))

  # large
  expect_silent(date_yq_breaks()(date_yq(1950:2021, 1)))
  expect_silent(date_ym_breaks()(date_ym(1990:2021, 1)))

  expect_silent(date_yw_breaks()(date_yw(1910, c(1, 50))))
  expect_silent(date_yw_breaks()(date_yw(1910:2221, 1)))

  expect_silent(date_yq_breaks()(date_yq(NA, NA)))
  expect_silent(date_ym_breaks()(date_ym(NA, NA)))
  expect_silent(date_yw_breaks()(date_yw(NA, NA)))
})
