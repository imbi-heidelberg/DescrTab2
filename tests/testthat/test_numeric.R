context("Output tables to the console")
library(magrittr)

test_that("numeric output does not produce errors.", {
  expect_error(descr(iris) %>% print(silent=T, print_format = "numeric"), NA)
  expect_error(
    descr(
      iris,
      "Species",
      group_labels = list(setosa = "My custom group label"),
      var_options = list(Sepal.Length = list(label = "My custom variable label"))
    ) %>% print(silent=T, print_format = "numeric"),
    NA
  )
})

verify_output(
  "../console/print_numeric_single.txt",
  descr(iris) %>% print(print_format = "numeric")
)
verify_output(
  "../console/print_numeric_group.txt",
  descr(
    iris,
    "Species",
    group_labels = list(setosa = "My custom group label"),
    var_options = list(Sepal.Length = list(label = "My custom variable label"))
  ) %>% print(print_format = "numeric")
)
