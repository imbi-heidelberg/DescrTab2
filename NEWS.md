# Version 2.1.10

* Fix bug where confidence intervals for differences in rates where displayed as differences in row-proportions instead of column-proportions
* Make error message for improperly matched data more precise

# Version 2.1.9

* Fix a bug where ordered factors were not correctly converted to rank data for non-parametric testing purposes

# Version 2.1.8

* Add check in .meanCIlower and .meanCIupper for non-constantness of data

# Version 2.1.7

* Fixed a bug where indices could not be specified inside var_options
* fixed bug with printing CI abbreviations in tex mode

# Version 2.1.6

* Add hline between footnotes and table in tex mode
* make pagebreak alogrithm less aggressive

# Version 2.1.5

* Fix bug with group_labels that lead to group N numbers not being dispalyed.

# Version 2.1.4

* Fix major bug where percentages were not calculated correctly if factor levels are omitted.
* Fix bug with confidence interval functions 

# Version 2.1.3

* Added lapply_descr function which allows applying descr to a list of datasets

# Version 2.1.2

* Major code refactoring, removing alot of code duplication
* The confidence interval column now behaves more similarily to the p-value colum and now supports footnotes.
* Fixed github actions and updated documentation
* Added Fisher's exact test for KxL tables
* Added exact binomial test for 1xL tables

# Version 2.1.1

* Fixed some issue in the usage documentation where a code chuck was accidentally hidden
* Some work on github actions

# Version 2.1

* knit_print now works properly. R chunks in .Rmd documents don't need the results = 'asis' option anymore.
* Tables can now have captions

# Version 2.0.13

* LaTeX tables will now properly wrap the names of long factor levels and long variable names.

# Version 2.0.12

* .mean now returns NA_real_ in cases were NaN was previously returned.

# Version 2.0.11

* Fixed a bug with paired data, where some of the pairs contain missing. In some cases, tests were not calculated and a misleading error message was produced.

# Version 2.0.10

* Added the ability to suppress the "Total" column via the format_options(print_Total = FALSE) option.
* Exact McNemars test will now calculate confidence intervals for rate differences by leveraging the distribution of the
test statistic.
* Using (non-exact) McNemars test will now produce a warning, that the confidence intervals for the differences in rates do not consider the paired structure of the data.
* Datasets containing variables which inherit from the "Date" class are now automatically converted to factors.

# Version 2.0.9

* Fixed a bug with print_CI = FALSE option in format_options.

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
