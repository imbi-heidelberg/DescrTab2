context("Output html tables in html_document .Rmd files")

test_on_cran <- TRUE

test_that("knit an html file with DescrTab2 tables", {
  skip_on_cran()
  expect_type(
    rmarkdown::render(
      "../rmds/html_doc.Rmd",
      clean = TRUE,
      quiet = TRUE,
      output_dir = ifelse(isTRUE(test_on_cran), tempfile(), NULL)
    ),
    "character"
  )
})
