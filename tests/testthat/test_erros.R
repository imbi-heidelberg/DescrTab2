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

dat <- tibble(a=list(a=1), b=list(b=1))
test_that("dataset with non-numeric non-factor argument produces errors",
          expect_error(descr(dat)))
