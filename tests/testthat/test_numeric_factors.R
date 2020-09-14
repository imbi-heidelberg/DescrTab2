context("Test if continuous tests and summary statistics work for factors.")

library(magrittr)

dat <- iris[iris$Species != "setosa", ]
dat$Species <- factor(as.character(dat$Species))
dat$cat_var <- c(1, 2) %>% sample(100, T) %>% factor()


test_that("summary statistics for factors work",
          expect_error(
            descr(
              dat,
              "Species",
              summary_stats_cat = list(
                mean = DescrTab2:::.factormean,
                sd = DescrTab2:::.factorsd,
                Q1 = DescrTab2:::.factorQ1,
                Q3 = DescrTab2:::.factorQ3,
                min = DescrTab2:::.factormin,
                max = DescrTab2:::.factormax,
                mode = DescrTab2:::.mode
              ),
              format_summary_stats = list(
                N = function(x)
                  format(x, digits = 2, scientific = 3),
                Nmiss = function(x)
                  format(x, digits = 2, scientific = 3),
                mean = function(x)
                  format(x, digits = 2, scientific = 3),
                sd = function(x)
                  format(x, digits = 2,
                         scientific = 3),
                median = function(x)
                  format(x, digits = 2, scientific = 3),
                Q1 =
                  function(x)
                    format(x, digits = 2, scientific = 3),
                Q3 = function(x)
                  format(x, digits
                         = 2, scientific = 3),
                min = function(x)
                  format(x, digits = 2, scientific = 3),
                max =
                  function(x)
                    format(x, digits = 2,      scientific = 3),
                CI = function(x)
                  format(x,
                         digits = 2, scientific = 3),
                mode = function(x)
                  format(x,
                         digits = 2, scientific = 3)
              ),
              var_options = list(cat_var = list(test_override  = "Welchs two-sample t-test"))
            ) %>% print(silent = T)
            ,
            NA
          ))
