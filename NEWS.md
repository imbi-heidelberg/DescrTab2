# Version 2.0.4

* All tests that previously used continuity correction by default do _not_ use continuity correction anymore.
* Added the ``additional_test_args`` argument to ``test_options``. This lets the user pass arguments to the underlying test functions, e.g. ``additional_test_args = list(correct=TRUE)`` to request continuity correction in ``chisq.test``.


# Version 2.0.3
* First cran release.
