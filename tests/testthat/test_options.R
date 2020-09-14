context("Test various options")
library(magrittr)

dat <- iris[, c("Species", "Sepal.Length")]
dat %<>% mutate(animal = c("Mammal", "Fish") %>% rep(75) %>% factor())
dat %<>% mutate(food = c("fries", "wedges") %>% sample(150, T) %>% factor())


test_that("Confidence intervals",
          expect_error(
            descr(
              dat %>% select(-"Species"),
              "animal",
              test_options = list(exact = T, nonparametric = T)
            ) %>%  print(silent = T),
            NA
          ))


test_that("Ommit summary stats",
          expect_error(
            descr(
              dat,
              "Species",
              summary_stats_cont = list(
                N = DescrTab2:::.N,
                Nmiss = DescrTab2:::.Nmiss,
                mean =
                  DescrTab2:::.mean,
                sd = DescrTab2:::.sd,
                median = DescrTab2:::.median,
                min = DescrTab2:::.min,
                max =
                  DescrTab2:::.max
              )
            ) %>% print(silent = T),
            NA
          ))

test_that("No p",
          expect_error(
            descr(dat, "animal", format_options = list(print_p = F, print_CI = F)) %>%  print(silent = T),
            NA
          ))

test_that("Per-variable options",
          expect_error(
            descr(iris, "Species", var_options = list(
              Sepal.Length = list(
                format_summary_stats = list(
                  mean = function(x)
                    formatC(x, digits = 4)
                ),
                test_options = c(nonparametric = T)
              )
            )) %>%  print(silent = T),
            NA
          ))


test_that("print_test_names works",
          expect_type(print_test_names(), "character"))

dat2 <- iris
dat2$cat_var <- c(1, 2) %>% sample(150, T) %>% factor()
dat2 <- dat2[, c("Species", "cat_var")]

test_that("cat_summary_stats works",
          {
            expect_error(descr(
              dat2,
              "Species",
              summary_stats_cat = list(mean = DescrTab2:::.factormean)
            ) %>% print(silent = T),
            NA)
            expect_error(
              descr(
                dat2,
                "Species",
                summary_stats_cat = list(mean = DescrTab2:::.factormean)
              ) %>% print(silent = T, print_format = "numeric"),
              NA
            )
          })


test_that("combine_mean_sd works",
          expect_error(descr(
            iris, format_options = c(combine_mean_sd = T)
          ) %>% print(silent = T),
          NA))

test_that("warnings about unused variable names work",
          {
          expect_warning(descr(iris, var_labels=c(a="b")))
          expect_warning(descr(iris, var_options=c(a="b")))
          }
          )

test_that("function list misconfiguration leads to error",
          {
            expect_error(descr(iris, summary_stats_cont="mean"))
            expect_error(descr(iris, summary_stats_cat="mean"))
            expect_error(descr(iris, format_p="mean"))
            expect_error(descr(iris, format_summary_stats="mean"))
          }
)

test_that("check if print_red_na option works",
          expect_type(capture.output(descr(iris) %>% print(print_format="numeric", print_red_NA=T)),"character") )









