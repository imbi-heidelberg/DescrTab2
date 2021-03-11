context("Output html tables in html_document .Rmd files")

# write_in_tmpfile_for_cran() <- FALSE

test_that(
  "knit an html file with DescrTab2 tables", {
    skip_on_cran()
    expect_type(
      rmarkdown::render(
        "../rmds/html_doc.Rmd",
        clean = TRUE,
        quiet = TRUE,
        output_dir = if(isTRUE(write_in_tmpfile_for_cran())) tempfile() else NULL
      ),
      "character"
    )
  })
