context("date_xx_scale")


test_that("date_xx_scale works as expected", {

  x <- seq(date_yq(2010, 1), date_yq(2020, 2))
  date_yq_breaks()(x)

  x <- seq(date_yq(1900, 1), date_yq(2020, 2))
  date_yq_breaks()(x)

  x <- seq(date_yq(2002, 3), date_yq(2003, 3))
  date_yq_breaks()(x)


  x <- seq(date_yq(2002, 3), date_yq(2002, 4))
  date_yq_breaks()(x)



})
