# dint 2.1.0.9000

# dint 2.0.0.9000

* added **ggplot2** scales for dint data types
* added predicates to check whether dates correspond to the boundaries of 
  years or quarters (`is_quarter_bounds()`, `is_year_bounds()`, 
  `is_first_of_quarter()`, etc...)
* export methods for `unique` and `summary` that were introduced in the last 
  version but mistakenly not exported
* One can now subtract two `date_xx` of the same subclass from each other 
  (the result is an `integer`)
* added `as_yearmon()` and `as_yearqtr()` for converting to **zoo** S3 classes
* Removed functions that were deprecated with dint 2.0.0
* added `Sys.date_yq()`, `Sys.date_ym()` and `Sys.date_yw()` to get the current
  quarter, month or isoweek.
* lots of small fixes


# dint 2.0.0

* **breaking**: formatting `date_xx` objects now uses placeholders similar to
  `base::strptime()` (e.g `%Y`, `%m`, `%q`, etc...) instead of presets. The
  new implementation is also noticeably faster than the old.
* **breaking**: all `first_day_of_*()` and `format_date_y*()` functions now
  have shorter names: `first_of_*()` and `format_y*()`. The original names
  will still work until the next release but give a deprecation warning.
* Added support for `c()`, `min()`, `max()`, `range()`
* You can now directly supply a numeric year to `first_of_year()` and 
  `last_of_year()` (e.g. `first_of_year(2018)`)
* added `date_yw` for storing isoweeks and modified existing functions to 
  accommodate for them.
* added `[` method for `date_xx` objects that preserves class attribute when
  subsetting
* `%y+%` / `%y-%` can add/subtract years from date_xx objects
* added the predicates, `is_first_of_quarter()`, `is_last_of_quarter()`, and
  `is_quarter_bounds()`.
* `seq()` methods now support a `by` argument




# dint 1.0.0

* First stable version
