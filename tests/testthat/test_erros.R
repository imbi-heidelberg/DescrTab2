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



test_that("warnings when lists which are supposed to be named dont have names are produced",
          {
            expect_warning(descr(iris, group_labels=c("a")))
            expect_warning(descr(iris, format_options = c("a")))
            expect_warning(descr(iris, test_options = c("a")))
            expect_warning(descr(iris, summary_stats_cat = list(mean)))
            expect_warning(descr(iris, format_summary_stats = list(formatC)))
          }
          )

test_that("error is produces when summary_stats lists are misspecified",
          {
            expect_error(descr(iris, summary_stats_cat = list("mean")))
            expect_error(descr(iris, summary_stats_cont = list("mean")))
            expect_condition(descr(iris, var_options = list(a=list(summary_stats=list("mean")))))
            expect_error(descr(iris, var_options = list(Sepal.Width = list(summary_stats=list("mean")))))
          })

test_that("error is produced when format_p in var_options is misspecified",
          expect_error(descr(iris, var_options = list(Sepal.Width = list(format_p=list("mean"))))))

test_that("error is produced reshape_rows is misspecified",
          {
            expect_error(descr(iris, reshape_rows=list(list(3))))
            expect_error(descr(iris, reshape_rows=c(a=3)))
          }
)

test_that("test override is check",
          expect_warning(descr(iris, var_options = list(Sepal.Length = list( test_override="abc"  ) )  ))
          )



test_that("test error catch mechanism works",
          {
            expect_message(descr(iris, "Species",  var_options = list(
              Sepal.Length = list(test_override = "Welchs two-sample t-test")
            )))
            expect_warning(descr(iris,  var_options = list(
              Species = list(test_override = "Pearsons chi-squared test")
            )))
          })

dat <- sleep[, c("extra", "group")] %>% mutate(cat= factor(c(rep("a", 10), rep("b", 10))))
test_that("forgotten indices in paired tests lead to errors",
          {
            expect_message(descr(dat %>% select(-cat), "group", test_options = list(paired=T)) %>% print(silent=T))
            expect_message(descr(dat %>% select(-extra), "group", test_options = list(paired=T)) %>% print(silent=T))
          }
          )







