

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
