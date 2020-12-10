context("Output html tables in html_document .Rmd files")

test_that(
  "knit an html file with DescrTab2 tables",{
  skip_on_cran()
  expect_type(
    rmarkdown::render("../rmds/html_doc.Rmd", clean = T, quiet = T),
    "character"
  )
  }

)
