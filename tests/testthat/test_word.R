


test_that(
  "knit a word file with DescrTab2 tables",{
    skip_on_cran()
    expect_type(
      rmarkdown::render(
        "../rmds/word_doc.Rmd",
        clean = TRUE,
        quiet = TRUE,
        output_dir = if(isTRUE(write_in_tmpfile_for_cran())) tempfile() else NULL
      ),
      "character"
    )
  }
)


test_that("Outputformat word produces no errors",{
  expect_error(descr(
    iris,
    "Species",
    group_labels = list(setosa = "My custom group label"),
    var_options = list(Sepal.Length = list(label = "My custom variable label"))
  ) %>% print(print_format="word", silent=FALSE), NA)
})
