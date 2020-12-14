context("Test various options")
library(magrittr)

dat <- iris[, c("Species", "Sepal.Length")]
dat %<>% mutate(animal = c("Mammal", "Fish") %>% rep(75) %>% factor())
dat %<>% mutate(food = c("fries", "wedges") %>% sample(150, TRUE) %>% factor())


test_that("Confidence intervals",
          {
            expect_error(descr(
              dat %>% select(-"Species"),
              "animal",
              test_options = list(exact = TRUE, nonparametric = TRUE)
            ) %>%  print(silent = TRUE),
            NA)

            expect_error(descr(
              dat %>% select(-"Species"),
              "animal",
              test_options = list(exact = TRUE, nonparametric = TRUE)
            ) %>%  print(silent = TRUE, print_format="numeric"),
            NA)

            expect_error(descr(
              dat %>% select(-"Species") %>% mutate(all_na = NA_real_),
              "animal",
              ) %>%  print(silent = TRUE),
            NA)

            expect_error(descr(
              dat %>% select(-"Species") %>% mutate(all_na = NA_real_),
              "animal",
            ) %>%  print(silent = TRUE, print_format="numeric"),
            NA)

            expect_error(descr(
              dat %>% select(-"Species") %>% mutate(all_na = NA_character_),
              "animal",
            ) %>%  print(silent = TRUE, print_format="numeric"),
            NA)

          })




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
            ) %>% print(silent = TRUE),
            NA
          ))

test_that("No p",
          {
            expect_error(
              descr(dat, "animal", format_options = list(
                print_p = FALSE, print_CI = FALSE
              )) %>%  print(silent = TRUE, print_format = "console"),
              NA
            )

            expect_error(
              descr(dat, "animal", format_options = list(
                print_p = FALSE, print_CI = FALSE
              )) %>%  print(silent = TRUE, print_format = "tex"),
              NA
            )

            expect_error(
              descr(dat, "animal", format_options = list(
                print_p = FALSE, print_CI = FALSE
              )) %>%  print(silent = TRUE, print_format = "word"),
              NA
            )

            expect_error(
              descr(dat, "animal", format_options = list(
                print_p = FALSE, print_CI = FALSE
              )) %>%  print(silent = TRUE, print_format = "html"),
              NA
            )


          })

test_that("Per-variable options",
          {
          expect_error(
            descr(iris, "Species", var_options = list(
              Sepal.Length = list(
                format_summary_stats = list(
                  mean = function(x)
                    formatC(x, digits = 4)
                ),
                test_options = c(nonparametric = TRUE)
              )
            )) %>%  print(silent = TRUE),
            NA
          )

          expect_error(
            descr(iris, var_options = list(
              Species = list(
                format_summary_stats = list(
                  mean = function(x)
                    formatC(x, digits = 4)
                ),
                test_options = c(include_group_missings_in_test = TRUE)
              )
            )) %>%  print(silent = TRUE),
            NA
          )

          expect_error(descr(iris, var_options = list(
            Sepal.Length = list(
              format_summary_stats = list(
                mean = function(x)
                  formatC(x, digits = 4)
              ),
              format_p = formatC,
              reshape_rows = list(`mean pm sd` = list(
                args = c("mean", "sd"),
                fun = function(mean, sd)
                  paste0(mean, " \u00B1 ", sd)
              )),
              test_options = c(include_group_missings_in_test = TRUE)
            )
          )) %>% print(silent=TRUE), NA)

          }
          )


test_that("print_test_names works",
          expect_type(print_test_names(), "character"))

dat2 <- iris
dat2$cat_var <- c(1, 2) %>% sample(150, TRUE) %>% factor()
dat2 <- dat2[, c("Species", "cat_var")]

test_that("cat_summary_stats works",
          {
            expect_error(descr(
              dat2,
              "Species",
              summary_stats_cat = list(mean = DescrTab2:::.factormean)
            ) %>% print(silent = TRUE),
            NA)
            expect_error(
              descr(
                dat2,
                "Species",
                summary_stats_cat = list(mean = DescrTab2:::.factormean)
              ) %>% print(silent = TRUE, print_format = "numeric"),
              NA
            )
          })


test_that("combine_mean_sd works",
          expect_error(descr(
            iris, format_options = c(combine_mean_sd = TRUE)
          ) %>% print(silent = TRUE),
          NA))

test_that("warnings about unused variable names work",
          {
          expect_warning(descr(iris, var_labels=c(a="b"))%>%
                           print(silent = TRUE))
          expect_warning(descr(iris, var_options=c(a="b"))%>%
                           print(silent = TRUE))
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
          expect_type(capture.output(descr(iris) %>% print(print_format="numeric", print_red_NA=TRUE)),"character") )

test_that("format_options in var_options is properly filled with default arguments",
          {
            expect_error(descr(iris, var_options = list(Sepal.Length = list(
              format_options = (print_p = FALSE)
            ))) %>%
              print(silent = TRUE), NA)

          })

test_that("reshape_rows in var_options is properly filled with default arguments",
          {
            expect_error(descr(iris, var_options = list(Sepal.Length = list(
              reshape_rows = list(`Q1 - Q3` = list(
                args = c("Q1", "Q3"),
                fun = function(Q1, Q3)
                  paste0(Q1, " -- ", Q3)
              ))
            ))) %>%
              print(silent = TRUE)
            , NA)
            expect_error(descr(iris, var_options = list(Sepal.Length = list(
              format_options = list(combine_mean_sd = TRUE)
            ))) %>%
              print(silent = TRUE)
            , NA)

          })


test_that("Special summary stats for 1 variable work",
          {
            expect_error(descr(iris,
                               var_options = list(Sepal.Length = list(
                                 summary_stats = list(mean =
                                                        DescrTab2:::.mean, sd = DescrTab2:::.sd)
                               ))) %>%
                                 print(silent = TRUE), NA)

            expect_warning(descr(iris,
                                 var_options = list(Species = list(
                                   summary_stats = list(mean =
                                                          DescrTab2:::.factormean)
                                 ))) %>%
                             print(silent = TRUE))

          })



test_that("Settings to determine the number of digits for perecent numbers work",
          {
          expect_error(descr(iris, format_options=list(percent_accuracy=0.1)) %>% print(silent=TRUE), NA)
          }
            )



