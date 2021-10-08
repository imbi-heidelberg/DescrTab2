x <- c(rep(TRUE, 40), rep(FALSE, 20))
y <- c(rep(TRUE, 35), rep(FALSE, 25))

test_that(
  "farrington.manning function does not produce errors",
  {
    expect_error(farrington.manning(x,y), NA)
    expect_error(farrington.manning(x,y, -.3), NA)
    expect_error(farrington.manning(x,y, .3), NA)
    expect_error(farrington.manning(x,y, 0.3, "less"), NA)
    expect_error(farrington.manning(x,y, 0.3, "greater"), NA)
    expect_error(farrington.manning(x,y, 0.3, "two.sided"), NA)
  }
)

test_that(
  "farrington.manning function produces error for misspecified input",
  {
    expect_error(farrington.manning(c(1,2,3),y))
    expect_warning(farrington.manning(c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,NA),y, -.3))
    expect_error(farrington.manning(x,y, 2, "less"))
    expect_error(farrington.manning(x,y, 0.3, "abc"))
    expect_error(farrington.manning(x,y, 0.3, "greater", 2))
    expect_error(farrington.manning(x,y, -2, "two.sided"))
  }
)
