context("Output flextables tables in word_document .Rmd files")

test_that(
  "knit a word file with DescrTab2 tables",
  expect_type(
    rmarkdown::render("../rmds/word_doc.Rmd", clean = T, quiet = T),
    "character"
  )
)
