context("accessors")


test_that("get_* returns integers", {
  d <- Sys.Date()
  ym <- date_ym(2018, 1)
  yq <- date_yq(2018, 1)


  expect_true(is.integer(get_month(d)))
  expect_true(is.integer(get_month(ym)))
  expect_true(is.integer(get_month(yq)))

  expect_true(is.integer(get_quarter(d)))
  expect_true(is.integer(get_quarter(ym)))
  expect_true(is.integer(get_quarter(yq)))

  expect_true(is.integer(get_year(d)))
  expect_true(is.integer(get_year(ym)))
  expect_true(is.integer(get_year(yq)))

})




test_that("get_week behaves like lubridate::isoweek", {

    # from https://en.wikipedia.org/wiki/ISO_week_date
    tdat <- read.csv2(textConnection(
    "2005-01-01; 	2004-W53;
     2005-01-02; 	2004-W53;
     2005-12-31; 	2005-W52;
     2007-01-01; 	2007-W01; 	Both years 2007 start with the same day.
     2007-12-30; 	2007-W52;
     2007-12-31; 	2008-W01;
     2008-01-01; 	2008-W01; 	Gregorian year 2008 is a leap year. ISO year 2008 is 2 days shorter: 1 day longer at the start, 3 days shorter at the end.
     2008-12-28; 	2008-W52; 	ISO year 2009 begins three days before the end of Gregorian 2008.
     2008-12-29; 	2009-W01;
     2008-12-30; 	2009-W01;
     2008-12-31; 	2009-W01;
     2009-01-01; 	2009-W01;
     2009-12-31; 	2009-W53; 	ISO year 2009 has 53 weeks and ends three days into Gregorian year 2010.
     2010-01-01; 	2009-W53;
     2010-01-02; 	2009-W53;
     2010-01-03; 	2009-W53;
     2010-01-04; 	2010-W01;
    "
    ),
      stringsAsFactors = FALSE,
      header = FALSE
    )


  tdat[[1]] <- as.Date(trimws(as.character(tdat[[1]])))
  tdat[[2]] <- trimws(tdat[[2]])


  expect_identical(
    format(as_date_yw(tdat[[1]])),
    tdat[[2]]
  )


})
