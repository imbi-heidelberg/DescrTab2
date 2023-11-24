library(dplyr, quietly=TRUE, warn.conflicts=FALSE)
dat <- iris[, c("Species", "Sepal.Length")]
dat <- dat |>  mutate(animal = c("Mammal", "Fish") |> rep(75) %>% factor())
dat <- dat |>  mutate(food = c("fries", "wedges") |> sample(150, TRUE) |> factor())

test_that("knit a tex file with DescrTab2 tables", {
  skip_on_cran()
  # skip_on_ci()
  suppressWarnings(expect_type(
    rmarkdown::render(
      "../rmds/tex_doc.Rmd",
      clean = TRUE,
      quiet = TRUE,
      output_dir = if(isTRUE(write_in_tmpfile_for_cran())) tempfile() else NULL
    ),
    "character"
  ))
})

test_that("Outputformat .tex produces no errors",{
  expect_error(capture_output(descr(
    iris,
    "Species",
    group_labels = list(setosa = "My custom group label"),
    var_options = list(Sepal.Length = list(label = "My custom variable label"))
  ) |> print(print_format="tex", silent=FALSE)), NA)
  expect_error(capture_output(descr(
    dat,
    "animal",
    group_labels = list(setosa = "My custom group label"),
    format_options = list(print_CI=TRUE),
    var_options = list(Sepal.Length = list(label = "My custom variable label"))
  ) |>  print(print_format="tex", silent=FALSE)), NA)
})
