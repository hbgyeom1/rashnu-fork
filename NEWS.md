# rashnu 0.1.2

* Added new functions for sample size and power calculation:
  - `one_mean_size()`
  - `two_mean_size()`
  - `k_mean_size()`
  - `one_prop_size()`
  - `two_prop_size()`
  - `pair_prop_size()`
  - `k_prop_size()`
  - `coxph_size()`
  - `or_size()`
  - `sccs_size()`
  - `one_norm_size()`
  - `one_bino_size()`

* Added Rstudio Addin "Power and Sample Size Calculator"

# rashnu 0.1.1

* Adjusted `numericInput` step size in the Shiny interface for finer control over input values.

# rashnu 0.1.0

## New features

* Initial release of `rashnu` package.
* Includes interactive Shiny app `rashnuBasic()` for sample size calculation.
* Supports:
  - Two-group non-inferiority trials
  - Two-group superiority trials (Lakatos method)
  - One-sample survival test (transformation-based)

## Internal improvements

* Added unit tests using `testthat`.
* Ensured all code is ASCII-compliant and CRAN-ready.
