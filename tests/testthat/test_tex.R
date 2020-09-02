context("Output tex tables in pdf_document .Rmd files")

test_that(
  "knit a tex file with DescrTab2 tables",
  expect_type(
    rmarkdown::render("../rmds/tex_doc.Rmd", clean = T, quiet = T),
    "character"
  )
)
