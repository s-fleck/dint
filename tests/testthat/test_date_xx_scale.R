context("date_xx_scale")


test_that("date_yq_breaks doesnt fail for a variety of inputs", {

  x <- rep(date_yq(2018, 1), 5)
  expect_silent(date_yq_breaks()(x))

  qs <- 1:4
  ys <- 1800:2200
  nas <- c(0, 2, 5)
  pad <- 0:4

  for(i in 1:100){
    y <- sample(ys, 2, replace = TRUE)
    q <- sample(qs, 2, replace = TRUE)
    p <- sample(pad, 2, replace = TRUE)
    na <- sample(nas, 1)
    x <- seq(date_yq(y[[1]], q[[1]]), date_yq(y[[2]], q[[2]]))
    x[sample(1:length(x), na, replace = TRUE)] <- NA
    expect_silent(date_yq_breaks()(x))
  }

  ys <- 2000:2010


  for(i in 1:100){
    y <- sample(ys, 2, replace = TRUE)
    q <- sample(qs, 2, replace = TRUE)
    p <- sample(pad, 2, replace = TRUE)
    na <- sample(nas, 1)
    x <- seq(date_yq(y[[1]], q[[1]]), date_yq(y[[2]], q[[2]]))
    x[sample(1:length(x), na, replace = TRUE)] <- NA
    expect_silent(date_yq_breaks()(x))
  }

  ys <- 2000:2005

  for(i in 1:100){
    y <- sample(ys, 2, replace = TRUE)
    q <- sample(qs, 2, replace = TRUE)
    p <- sample(pad, 2, replace = TRUE)
    na <- sample(nas, 1)
    x <- seq(date_yq(y[[1]], q[[1]]), date_yq(y[[2]], q[[2]]))
    x[sample(1:length(x), na, replace = TRUE)] <- NA
    expect_silent(date_yq_breaks()(x))
  }
})



test_that("date_ym_breaks doesnt fail for a variety of inputs", {
  skip("not yet")
  x <- date_ym(2018, 1:12)
  date_ym_breaks()(x)
})




test_that("date_ym_trans is reversable", {
  x <- seq(date_ym(1950, 1), date_ym(2050, 1))
  trn <- date_ym_trans$transform(x)
  inv <- date_ym_trans$inverse(trn)
  expect_identical(x, inv)
})
