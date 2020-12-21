context("Test miscellaneous functions")
library(magrittr)

test_on_cran <- FALSE


test_that("ignore_unused_args works as expected", {
  expect_warning(expect_equal(
    ignore_unused_args(chisq.test, list(x=factor(c(
      1, 0, 1, 0, 1
    )), y=factor(c(
      1, 0, 0, 0, 1
    )) , correct = FALSE))$statistic,
    chisq.test(factor(c(1, 0, 1, 0, 1)), factor(c(1, 0, 0, 0, 1)) , correct = FALSE)$statistic
  ))
  expect_warning(expect_equal(
    ignore_unused_args(chisq.test, list(x=factor(c(
      1, 0, 1, 0, 1
    )), y=factor(c(
      1, 0, 0, 0, 1
    )) , correct = TRUE))$statistic,
    chisq.test(factor(c(1, 0, 1, 0, 1)), factor(c(1, 0, 0, 0, 1)) , correct = TRUE)$statistic
  ))
  expect_identical(
    ignore_unused_args(stats::t.test, list(x=c(1,2,3,4,5), alternative="less")),
    t.test(c(1,2,3,4,5), alternative = "less")
  )
})
