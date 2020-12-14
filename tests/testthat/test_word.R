context("Output flextables in word_document .Rmd files")

test_on_cran <- TRUE

test_that(
  "knit a word file with DescrTab2 tables",{
    skip_on_cran()
    expect_type(
      rmarkdown::render("../rmds/word_doc.Rmd", clean = TRUE, quiet = TRUE, output_dir = ifelse(isTRUE(test_on_cran), tempfile(), NULL)),
      "character"
    )
  }
)
