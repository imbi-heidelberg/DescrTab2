context("Test if farrington.manning function works.")


x <- c(rep(T, 40), rep(F, 20))
y <- c(rep(T, 35), rep(F, 25))

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
    expect_warning(farrington.manning(c(T,T,T,T,F,F,NA),y, -.3))
    expect_error(farrington.manning(x,y, 2, "abc"))
    expect_error(farrington.manning(x,y, 0.3, "greater", 2))
    expect_error(farrington.manning(x,y, -2, "two.sided"))
  }
)
