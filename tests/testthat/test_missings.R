library(dplyr)
library(magrittr)

dat1 <- iris
dat1[1, "Sepal.Length"] <- NA
dat1$cat_var <-  c("a", "b") %>% sample(150, TRUE) %>% factor()

dat1[1, "cat_var"] <- NA

dat2 <- iris
dat2$Sepal.Length <- NA_real_

dat3 <- iris
dat3[1, "Species"] <- NA


test_that("Missings are handles properly", {
  expect_error(descr(dat1, "Species") %>%  print(silent=TRUE), NA)
  expect_error( descr(
    dat1,
    "Species",
    format_options = list(categorical_missing_percent_mode = "missing_as_regular_category")
  ) %>%  print(silent=TRUE), NA)
  expect_error( descr(
    dat1,
    "Species",
    format_options = list(categorical_missing_percent_mode = "missing_as_separate_category")
  ) %>%  print(silent=TRUE), NA)
  expect_error( descr(dat2, "Species") %>%  print(silent=TRUE), NA)
  expect_error( descr(
    dat2,
    "Species",
    format_options = list(categorical_missing_percent_mode = "missing_as_regular_category")
  ) %>%  print(silent=TRUE), NA)
  expect_error(descr(
    dat2,
    "Species",
    format_options = list(categorical_missing_percent_mode = "missing_as_separate_category")
  ) %>%  print(silent=TRUE), NA)
  expect_warning( descr(dat3, "Species") %>%  print(silent=TRUE) )
  expect_error( descr(dat3, "Species", format_options = list(omit_missings_in_group = FALSE)) %>%  print(silent=TRUE), NA)
  expect_error(descr(dat1, "Species", format_options = list(omit_missings_in_categorical_var =TRUE), test_options = list(include_group_missings_in_test=TRUE)))
  expect_error(descr(dat1, "Species", var_options=list(cat_var = list(format_options = list(omit_missings_in_categorical_var =TRUE), test_options = list(include_group_missings_in_test=TRUE)))))
  expect_error(descr(dat1, "Species", format_options = list(omit_missings_in_categorical_var =TRUE)) %>%  print(silent=TRUE), NA)
  expect_error(descr(dat2, "Species", format_options = list(omit_missings_in_categorical_var =TRUE)) %>%  print(silent=TRUE), NA)
})

test_that("Missings are handles properly with row-wise percentages", {
  expect_error(descr(dat1, "Species", format_options = list(row_percent=TRUE, Nmiss_row_percent = TRUE)) %>%  print(silent=TRUE), NA)
  expect_error( descr(
    dat1,
    "Species",
    format_options = list(categorical_missing_percent_mode = "missing_as_regular_category", row_percent=TRUE, Nmiss_row_percent = TRUE)
  ) %>%  print(silent=TRUE), NA)
  expect_error( descr(
    dat1,
    "Species",
    format_options = list(categorical_missing_percent_mode = "missing_as_separate_category", row_percent=TRUE, Nmiss_row_percent = TRUE)
  ) %>%  print(silent=TRUE), NA)
  expect_error( descr(dat2, "Species") %>%  print(silent=TRUE), NA)
  expect_error( descr(
    dat2,
    "Species",
    format_options = list(categorical_missing_percent_mode = "missing_as_regular_category", row_percent=TRUE, Nmiss_row_percent = TRUE)
  ) %>%  print(silent=TRUE), NA)
  expect_error(descr(
    dat2,
    "Species",
    format_options = list(categorical_missing_percent_mode = "missing_as_separate_category", row_percent=TRUE, Nmiss_row_percent = TRUE)
  ) %>%  print(silent=TRUE), NA)
  expect_warning(descr(dat3, "Species", format_options=list(row_percent=TRUE, Nmiss_row_percent = TRUE)) %>%  print(silent=TRUE) )
  expect_error(descr(dat3, "Species", format_options = list(omit_missings_in_group = FALSE, row_percent=TRUE, Nmiss_row_percent = TRUE)) %>%  print(silent=TRUE), NA)
  expect_error(descr(dat1, "Species", format_options = list(omit_missings_in_categorical_var =TRUE, row_percent=TRUE, Nmiss_row_percent = TRUE), test_options = list(include_group_missings_in_test=TRUE)))
  expect_error(descr(dat1, "Species", var_options=list(cat_var = list(format_options = list(omit_missings_in_categorical_var =TRUE), test_options = list(include_group_missings_in_test=TRUE)))))
  expect_error(descr(dat1, "Species", format_options = list(omit_missings_in_categorical_var =TRUE, row_percent=TRUE, Nmiss_row_percent = TRUE)) %>%  print(silent=TRUE), NA)
  expect_error(descr(dat2, "Species", format_options = list(omit_missings_in_categorical_var =TRUE, row_percent=TRUE, Nmiss_row_percent = TRUE)) %>%  print(silent=TRUE), NA)
})





