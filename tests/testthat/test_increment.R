context("increment")


test_that("increment.date_yq works as expected", {

  x <- as_date_yq(c(14, 101, 212341, 212344, 20151, 20164))

  expect_silent(xp1 <- increment(x, 1))
  expect_silent(xp5 <- increment(x, 5))
  expect_silent(xm1 <- increment(x, -1))
  expect_silent(xm5 <- increment(x, -5))

  expect_identical(xp1, as_date_yq(c(21, 102, 212342, 212351, 20152, 20171)))
  expect_identical(xp5, as_date_yq(c(31, 112, 212352, 212361, 20162, 20181)))
  expect_identical(xm1, as_date_yq(c(13, 94, 212334, 212343, 20144, 20163)))
  expect_identical(xm5, as_date_yq(c(3, 84, 212324, 212333, 20134, 20153)))

})



test_that("increment.date_ym", {

  x <- as_date_ym(c(201212, 201201, 001, -101, 11, 3))

  expect_silent(xp1 <- increment(x, 1))
  expect_silent(xp5 <- increment(x, 5))
  expect_silent(xm1 <- increment(x, -1))
  expect_silent(xm5 <- increment(x, -5))

  expect_identical(xp1, as_date_ym(c(201301, 201202, 2, -102, 12, 4)))
  expect_identical(xp5, as_date_ym(c(201305, 201206, 6, -106, 104, 8)))
  expect_identical(xm1, as_date_ym(c(201211, 201112, -112, -212, 10, 2)))
  expect_identical(xm5, as_date_ym(c(201207, 201108, -108, -208, 6, -110)))
})
