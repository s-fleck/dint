context("date_yw")




test_that("as_date_yw behaves as expected", {
    #* @testing get_isoweek

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
     2010-01-04; 	2010-W01;"
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

  expect_identical(
    get_isoweek(tdat[[1]]),
    as.integer(substr(tdat[[2]], 7, 8))
  )
})




test_that("format_yw behaves as expected", {
  expect_identical(format_yw(2015, 5), "2015-W05")
  expect_identical(format_yw(201505, format = "%Y.%V"), "2015.05")
  expect_identical(
    format_yw(as_date_yw(201505), format = "%y.%V"),
    "15.05"
  )
})
