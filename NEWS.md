# dint 1.0.0.9000

* breaking: formatting `date_xx` objects now uses placeholders similar to
  `base::strptime()` (e.g `%Y`, `%m`, `%q`, etc...) instead of presets. The
  new implementation is also faster than the old formatting functions.
* **breaking**: all `first_day_of()` have been renamed to `first_of()`. The old
  function names are deprecated but will still work until the next release.
* added `date_yw` for storing isoweeks and modified existing funtions to 
  accomodate for them.
* added `[` method for `date_xx` objects that preserves class attribute when
  subsetting
* `%y+%` / `%y-%` can add/subtract years from date_xx objects
* added the predicates `is_quarter_bounds()`, `is_first_day_of_quarter()`, 
  and `is_last_day_of_quarter()`




# dint 1.0.0

* First stable version
