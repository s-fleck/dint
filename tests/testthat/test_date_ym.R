context("date_ym")


test_that("date_ym works as expected", {

  expect_identical(
    as.integer(seq(as_date_ym(201802), as_date_ym(201904))),
    c(201802L, 201803L, 201804L, 201805L, 201806L, 201807L, 201808L,
      201809L, 201810L, 201811L, 201812L, 201901L, 201902L, 201903L,
      201904L)
  )

})
