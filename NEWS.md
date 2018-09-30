# dint 2.0.0.9000

# dint 2.0.0

* **breaking**: formatting `date_xx` objects now uses placeholders similar to
  `base::strptime()` (e.g `%Y`, `%m`, `%q`, etc...) instead of presets. The
  new implementation is also noticably faster than the old.
* **breaking**: all `first_day_of_*()` and `format_date_y*()` functions now
  have shorter names: `first_of_*()` and `format_y*()`. The original names
  will still work until the next release but give a deprecation warning.
* Added support for `c()`, `min()`, `max()`, `range()`
* You can now direclty supply a numeric year to `first_of_year()` and 
  `last_of_year()` (e.g. `first_of_year(2018)`)
* added `date_yw` for storing isoweeks and modified existing funtions to 
  accomodate for them.
* added `[` method for `date_xx` objects that preserves class attribute when
  subsetting
* `%y+%` / `%y-%` can add/subtract years from date_xx objects
* added the predicates, `is_first_of_quarter()`, `is_last_of_quarter()`, and
  `is_quarter_bounds()`.
* `seq()` methods now support a `by` argument




# dint 1.0.0

* First stable version
