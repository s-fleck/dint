context("format")



test_that("format date_yq works as expected", {
  td <- date_yq(2010:2019, 3)
  fm <- "Y%Y-y%y-q%q-%%yY"

  expect_identical(
    format(td, fm),
    c("Y2010-y10-q3-%10Y", "Y2011-y11-q3-%11Y", "Y2012-y12-q3-%12Y",
      "Y2013-y13-q3-%13Y", "Y2014-y14-q3-%14Y", "Y2015-y15-q3-%15Y",
      "Y2016-y16-q3-%16Y", "Y2017-y17-q3-%17Y", "Y2018-y18-q3-%18Y",
      "Y2019-y19-q3-%19Y")
  )
})



test_that("tokenize_format works as expected", {
  fm <- "Y%Y-y%y-q%q-%%yY"
  expect_identical(
    tokenize_format(fm),
    c("Y", "%Y", "-y", "%y", "-q", "%q", "-%", "%y", "Y")
  )

  fm <- "%Y-y%y-q%q-%%y"
  expect_identical(
    tokenize_format(fm),
    c("%Y", "-y", "%y", "-q", "%q", "-%", "%y")
  )
})


