library(Exact)

dat <- data.frame(
  a=sample(c("a", "b", "c"), 100, TRUE),
  b=sample(c("a", "b", "c"), 100, TRUE)
)

exact.test(dat)
