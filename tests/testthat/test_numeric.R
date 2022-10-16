library(magrittr, quietly = TRUE, warn.conflicts = FALSE)

dat <- iris[iris$Species != "setosa",]
dat$Species <- factor(as.character(dat$Species))
dat$cat_var <- c("a", "b") %>% rep(50) %>% factor()

custom_ttest2 <- list(
  name = "custom t-test",
  abbreviation = "custom",
  p = function(var, group) {
    return(t.test(var ~ group, data.frame(var = var, group = group))$p.value)
  }
)

test_that("numeric output does not produce errors.", {
  expect_error(descr(iris) %>% print(silent = TRUE, print_format = "numeric"), NA)
  expect_error(
    descr(
      iris,
      "Species",
      group_labels = list(setosa = "My custom group label"),
      var_options = list(Sepal.Length = list(label = "My custom variable label"))
    ) %>% print(silent = TRUE, print_format = "numeric"),
    NA
  )
  expect_error(descr(dat,
                     "Species") %>% print(silent = TRUE, print_format = "numeric"),
               NA)
  expect_error(descr(dat,
                    "Species",
                    format_options = list (print_CI = FALSE)) %>% print(silent = TRUE, print_format = "numeric"),
              NA)
  expect_error(
    descr(iris %>% mutate(Species = fct_collapse(Species, setosa = c("setosa", "versicolor"))),
    "Species",
    var_options = list(Sepal.Length = list(test_override = custom_ttest2))
  ) %>%
  print(print_format="numeric", silent = TRUE),
    NA
  )
})

verify_output(
  ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/print_numeric_single.txt"),
  descr(iris) %>% print(print_format = "numeric")
)
verify_output(
  ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/print_numeric_group.txt"),
  descr(
    iris,
    "Species",
    group_labels = list(setosa = "My custom group label"),
    var_options = list(Sepal.Length = list(label = "My custom variable label"))
  ) %>% print(print_format = "numeric")
)

verify_output(ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/print_numeric_CI.txt"),
              descr(dat,
                    "Species") %>% print( print_format = "numeric"))


test_that("Summary stats that produce non-numeric output lead to erros when numeric output format is requested",
          {
            expect_warning(
              expect_error(descr(iris, summary_stats_cat = list(abc=function(...){"abc"} )) %>% print(print_format="numeric"))
            )

            expect_warning(
              expect_error(descr(iris, summary_stats_cont = list(abc=function(...){"abc"} )) %>% print(print_format="numeric"))
            )
          }
          )











