context("Text if edge cases are handled properly")

library(dplyr)

dat <-
  iris %>% mutate(Sepal.Length = if_else(Species == "setosa", NA_real_, Sepal.Length)) %>%
  mutate(abc = if_else(row_number() == 1L, 2, Sepal.Length))

test_that("No test is performed when there is only one nonmissing observation in some group.",
          {
            expect_warning(descr(dat, "Species"))

          })
