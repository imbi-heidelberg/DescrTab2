library(magrittr, quietly = TRUE, warn.conflicts = FALSE)


test_that("ignore_unused_args works as expected", {
  expect_warning(expect_warning(
    expect_equal(
      ignore_unused_args(chisq.test, list(
        x = factor(c(1, 0, 1, 0, 1)),
        y = factor(c(1, 0, 0, 0, 1)) ,
        correct = FALSE
      ))$statistic,
      chisq.test(factor(c(1, 0, 1, 0, 1)), factor(c(1, 0, 0, 0, 1)) , correct = FALSE)$statistic
    )
  ))
  expect_warning(expect_warning(
    expect_equal(
      ignore_unused_args(chisq.test, list(
        x = factor(c(1, 0, 1, 0, 1)),
        y = factor(c(1, 0, 0, 0, 1)) ,
        correct = TRUE
      ))$statistic,
      chisq.test(factor(c(1, 0, 1, 0, 1)), factor(c(1, 0, 0, 0, 1)) , correct = TRUE)$statistic
    )
  ))
  expect_identical(ignore_unused_args(stats::t.test, list(
    x = c(1, 2, 3, 4, 5), alternative = "less"
  )),
  t.test(c(1, 2, 3, 4, 5), alternative = "less"))
})

test_that("format_freqs works",
          {
            expect_equal(
              format_freqs(1, 3, "both", NULL, "%"),
              "1 (33%)")
            expect_equal(
              format_freqs(1, 3, "only_absolute", NULL, "%"),
              "1")
            expect_equal(
              format_freqs(1, 3, "only_relative", NULL, "%"),
              "33%")
            expect_error(
              format_freqs(1, 3, "abc", NULL, "%")
            )
          }
)



test_that("write_in_tmpfile_for_cran function works",{
  if (!write_in_tmpfile_for_cran()){
  Sys.setenv("NOT_CRAN" = "")
  expect_true(write_in_tmpfile_for_cran())
  Sys.setenv("NOT_CRAN" = "true")
  expect_false(write_in_tmpfile_for_cran())
  }
})

test_that("lapply_descr works",
          {
            l <- list()
            for (i in 1:2){
              l <- append(l, list(iris))
            }
            expect_output(lapply_descr(l, group="Species"))
          })


test_that("strechSpace works in_minipage, even though i forgot why this is even implemented",{
  expect_true(grepl("setstretch", in_minipage("abc", "1cm", 1, TRUE), fixed = TRUE))
})




