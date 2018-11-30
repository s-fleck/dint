context("predicates")


test_that("quarter_bounds works as expected", {

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



test_that("year_bounds", {

  yqs <- first_of_year(2018)

  tdat <- data.frame(
    start = c(as.Date(c("2018-01-01", "2018-01-01", "2018-01-01", "2018-02-01"))),
    end   = c(as.Date(c("2018-01-01", "2018-12-31", "2019-12-31", "2018-04-05")))
  )

  expect_identical(
    is_first_of_year(tdat$start),
    c(TRUE, TRUE, TRUE, FALSE)
  )

  expect_identical(
    is_last_of_year(tdat$end),
    c(FALSE, TRUE, TRUE, FALSE)
  )

  expect_identical(
    is_year_bounds(tdat$start, tdat$end),
    c(FALSE, TRUE, FALSE, FALSE)
  )
})
