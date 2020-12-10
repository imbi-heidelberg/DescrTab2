context("Output flextables in word_document .Rmd files")

test_that(
  "knit a word file with DescrTab2 tables",{
    skip_on_cran()
    expect_type(
      rmarkdown::render("../rmds/word_doc.Rmd", clean = T, quiet = T),
      "character"
    )
  }
)
