context("predicates")


test_that("predicates works as expected", {

  yqs <- date_yq(2018, 1:4)

  tdat <- data.frame(
    start = c(first_of_quarter(yqs), as.Date(c("2018-03-31", "2018-02-14"))),
    end   = c(last_of_quarter(yqs), as.Date(c("2018-01-01", "2018-02-14")))
  )

  expect_identical(
    is_first_of_quarter(tdat$start),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  expect_identical(
    is_last_of_quarter(tdat$end),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  expect_identical(
    is_quarter_bounds(tdat$start, tdat$end),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})
