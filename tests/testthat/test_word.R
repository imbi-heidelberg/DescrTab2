context("Output flextables in word_document .Rmd files")



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
