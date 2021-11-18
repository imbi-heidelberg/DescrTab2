test_that("guess_ID_variable works properly", {
  expect_equal(guess_ID_variable(c(a=3, Id=2), TRUE), "Id")
  expect_equal(guess_ID_variable(c(a=3, id=2), TRUE), "id")
  expect_equal(guess_ID_variable(c(a=3, patid=2), TRUE), "patid")
  expect_equal(guess_ID_variable(c(a=3, `.id_`=2), TRUE), ".id_")
  expect_null(guess_ID_variable(c(a=3, id=2, patid=4), TRUE))
  expect_null(guess_ID_variable(c(a=3, b=2), TRUE))

  expect_warning(guess_ID_variable(c(a=3, Id=2)))
  expect_warning(guess_ID_variable(c(a=3, id=2)))
  expect_warning(guess_ID_variable(c(a=3, patid=2)))
  expect_warning(guess_ID_variable(c(a=3, `.id_`=2)))
  expect_warning(guess_ID_variable(c(a=3, id=2, patid=4)))
  expect_warning(guess_ID_variable(c(a=3, b=2)), NA)
})



test_that("extract_labels works",
{
a <- c(1, 2)
attr(a, "label") <- "b"
expect_true(identical(extract_labels(a), list(a = attr(a, "label"))))
})

test_that("unlabel works",
{
a <- c(1, 2)
attr(a, "label") <- "b"
expect_true(identical(unlabel(a), c(1, 2)) )
}
)

test_that("read_redcap_formatted works",
{
path_to_redcap_script <- system.file("examples", "testredcap.r", package = "DescrTab2")
expect_error(read_redcap_formatted(path_to_redcap_script), NA)
})

test_that("split_redcap_dataset works",
{
path_to_redcap_script <- system.file("examples", "testredcap.r", package = "DescrTab2")
dat <- read_redcap_formatted(path_to_redcap_script)
expect_error(d <- split_redcap_dataset(dat, guess_ID_variable(dat, TRUE)), NA)
})

test_that("read_sas_formatted works",
{
path_to_data <- system.file("examples", "testsas.sas7bdat", package = "DescrTab2")
pat_to_format <- system.file("examples", "formats.sas7bcat", package = "DescrTab2")
expect_error(read_sas_formatted(path_to_data, pat_to_format), NA)
}
)

test_that("list_freetext_markdown works",
{
dat  <- data.frame(Freetext = c("Some text", "More text"))
expect_error(list_freetext_markdown(dat), NA)
})

test_that("parse_formats works",
{
tmpfile <- tempfile()
write("proc format;
        value yn  1=\"yes\"
                  0=\"no\";
        value sex 1=\"female\"
                  0=\"male\";
        value $dummy \" \"=\"   \";
        value other_delim 0 = '0'
                          1 = '1';
        run;",tmpfile)
expect_warning(parse_formats(tmpfile))
}
)

test_that("codegen_load_all_sas_data",
{
expect_output(codegen_load_all_sas_data(system.file("examples", package = "DescrTab2")))
})


test_that("edge cases work in loadData.R",{
  a <- c(1, 2)
  class(a) <- "labelled"
  attr(a, "label") <- "a"
  expect_identical(unlabel(a), c(1,2))

})
