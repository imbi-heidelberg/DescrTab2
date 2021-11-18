library(magrittr, quietly = TRUE, warn.conflicts = FALSE)

dat <- iris[, c("Species", "Sepal.Length")]
dat %<>% mutate(animal = c("Mammal", "Fish") %>% rep(75) %>% factor())
dat %<>% mutate(food = c("fries", "wedges") %>% sample(150, TRUE) %>% factor())


test_that("Confidence intervals for differences work",
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
    dat ,
    "Species",
    format_options = list(print_CI = FALSE)
  ) %>%  print(silent = TRUE, print_format="numeric"),
  NA)

  expect_error(
    descr(
        dat ,
        "animal",
        format_options = list(print_CI = FALSE) 
      ),
      NA
  )

  expect_error(descr(
    dat %>% select(-"Species") ,
    "animal",
    format_options = list(print_CI = FALSE)
  ) %>%  print(silent = TRUE, print_format="console"),
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




test_that("Confidence intervals for differences work",
{
  descr(
    dat %>% select(-"Species") %>% mutate(all_na = NA_real_),
    "animal",
    format_options = list(print_CI = TRUE, print_p=FALSE)
  ) %>% print(print_format="html")

    descr(
    dat %>% select(-"Species") %>% mutate(all_na = NA_real_),
    "animal",
    format_options = list(print_CI = TRUE, print_p=FALSE)
  ) %>% print(print_format="tex")

      descr(
    dat %>% select(-"Species") %>% mutate(all_na = NA_real_),
    "animal",
    format_options = list(print_CI = TRUE, print_p=FALSE)
  ) %>% print(print_format="console")

  descr(
    dat %>% select(-"Species") %>% mutate(all_na = NA_real_),
    "animal",
    format_options = list(print_CI = TRUE, print_p=FALSE)
  ) %>% print(print_format="word")

    descr(
    dat %>% select(-"Species") %>% mutate(all_na = NA_real_),
    "animal",
    format_options = list(print_CI = TRUE, print_p=FALSE)
  ) %>% print(print_format="numeric")

})







dat  %<>% mutate(,
    only_one_level = factor(rep("first_level", nrow(dat))),
    everything_missing = rep(NA_real_, nrow(dat))
    )
test_that("Confidence intervals for edge cases work",
          {
              expect_warning(descr(dat, "animal"))
          })