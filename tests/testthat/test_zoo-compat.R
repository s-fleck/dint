context("zoo-compat")


test_that("zoo-compat works as expected", {
  q <- date_yq(2016, 1:4)
  qzoo <- as_yearqtr(q)
  expect_equal(as.numeric(qzoo), seq(2016.00, 2016.75, by = 0.25))
  rev <- as_date_yq(qzoo)
  expect_identical(rev, q)

  m <- date_ym(2016, 1:12)
  mzoo <- as_yearmon(m)
  expect_equal(as.numeric(mzoo), seq(2016.00, 2016 + 11/12, by = 1/12))
  rev <- as_date_ym(mzoo)
  expect_identical(rev, m)

  w <- date_yw(2016, 1:52)
  wzoo <- as_yearweek(w)
  expect_equal(as.numeric(wzoo), seq(2016.00, 2016 + 51/53, by = 1/53))
  rev <- as_date_yw(wzoo)
  expect_identical(rev, w)
})
