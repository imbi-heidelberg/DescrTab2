context("Test input that should produce errors.")

library(magrittr)
library(tibble)

test_that("Empty dataset produces error",
          {
            expect_error(descr(tibble()))
            expect_error(descr(tibble(group = 1), "group"))
          })


test_that("group misspecification leads to error",
          expect_error(descr(iris, c(1,2,3))))


test_that("Warnings for missing formatting functions are produced",{
  expect_warning(descr(iris, summary_stats_cat= list(mod=function(x)3)))
  expect_warning(descr(iris, var_options = list(Species=list(summary_stats = list(mod=function(x)3)))))
})

