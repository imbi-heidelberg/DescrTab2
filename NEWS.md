# Version 2.0.9

* Added the ability to suppress the "Total" column via the format_options(print_Total = FALSE) option.
* Exact McNemars test will now calculate confidence intervals for rate differences by leveraging the distribution of the
test statistic.
* Using (non-exact) McNemars test will now produce a warning, that the confidence intervals for the differences in rates do not consider the paired structure of the data.
* Datasets containing variables which inherit from the "Date" class are now automatically converted to factors.

# Version 2.0.8

* "&" signs in factor labels are now properly escaped in LaTeX code.

# Version 2.0.7

* Added the "combine_median_Q1_Q3" argument to format_options, which reshapes these summary statistics to "median (Q1, Q3)".

# Version 2.0.6

* Added the following members to format_options: percent_suffix, row_percent, Nmiss_row_percent and absolute_relative_frequency_mode. They can be used to control control how absolute and relative frequencies are displayed.

# Version 2.0.5

* Added documentation for confidence intervals.

# Version 2.0.4

* All tests that previously used continuity correction by default do _not_ use continuity correction anymore.
* Added the ``additional_test_args`` argument to ``test_options``. This lets the user pass arguments to the underlying test functions, e.g. ``additional_test_args = list(correct=TRUE)`` to request continuity correction in ``chisq.test``.

# Version 2.0.3
* First cran release.
