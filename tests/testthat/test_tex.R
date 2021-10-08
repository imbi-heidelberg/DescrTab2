

test_that("knit a tex file with DescrTab2 tables", {
  skip_on_cran()
  skip_on_travis()
  expect_type(
    rmarkdown::render(
      "../rmds/tex_doc.Rmd",
      clean = TRUE,
      quiet = TRUE,
      output_dir = if(isTRUE(write_in_tmpfile_for_cran())) tempfile() else NULL
    ),
    "character"
  )
})


test_that("Outputformat .tex produces no errors",{
  expect_error(capture_output(descr(
    iris,
    "Species",
    group_labels = list(setosa = "My custom group label"),
    var_options = list(Sepal.Length = list(label = "My custom variable label"))
  ) %>% print(print_format="tex", silent=F)), NA)
})
