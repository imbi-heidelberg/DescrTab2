library(dplyr)

dat <-
  iris %>% mutate(Sepal.Length = if_else(Species == "setosa", NA_real_, Sepal.Length)) %>%
  mutate(abc = if_else(row_number() == 1L, 2, Sepal.Length))

test_that("No test is performed when there is only one nonmissing observation in some group.",
          {
            expect_warning(descr(dat, "Species"))

          })


dat <- iris %>% mutate(cat= factor(rep(c("a", "b", "c"), 50)) )
test_that("No test is calculated if a test is requested that is not implemented",
          {
            expect_error(descr(dat, "Species", test_options = list(exact=TRUE, paired=TRUE, indices=c(1:50, 1:50, 1:50)),
                               format_options=list(print_Total=FALSE)), NA)
            expect_error(descr(dat, "Species", test_options = list(exact=TRUE, paired=FALSE),
                               format_options=list(print_Total=FALSE)), NA)
                         expect_error(descr(dat, "Species", test_options = list(exact=FALSE, paired=TRUE, indices=c(1:50, 1:50, 1:50)),
                                            format_options=list(print_Total=FALSE)), NA)
          }
          )


dat <- tibble(a=c("a", ""), b=factor(c("a", "")))
dat2 <- tibble(a=c("a", "(empty)"), b=factor(c("a", "")))
test_that("Empty strings are handled correctly",
          {
            expect_warning(expect_warning(expect_warning(descr(dat))))
            expect_error(descr(dat2))
          }
)
