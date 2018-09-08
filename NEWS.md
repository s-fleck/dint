# dint 1.0.0.9000

* **breaking**: formatting `date_xx` objects now uses placeholders similar to
  `base::strptime()` (e.g `%Y`, `%m`, `%q`, etc...) instead of presets. The
  new implementation is also faster than the old formatting functions.
* **breaking**: all `first_day_of_*()` have been renamed to `first_of_*()`. The 
  old function names are deprecated but will still work until the next release.
* You can now direclty supply a numeric year to `first_of_year()` and 
  `last_of_year()` (e.g. `first_of_year(2018)`)
* added `date_yw` for storing isoweeks and modified existing funtions to 
  accomodate for them.
* added `[` method for `date_xx` objects that preserves class attribute when
  subsetting
* `%y+%` / `%y-%` can add/subtract years from date_xx objects
* added the predicates, `is_first_of_quarter()`, `is_last_of_quarter()`, and
  `is_quarter_bounds()`.




# dint 1.0.0

* First stable version
