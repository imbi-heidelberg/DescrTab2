library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

dat <-
  iris %>% mutate(Sepal.Length = if_else(Species == "setosa", NA_real_, Sepal.Length)) %>%
  mutate(abc = if_else(row_number() == 1L, 2, Sepal.Length))

test_that("No test is performed when there is only one nonmissing observation in some group.",
          {
            expect_warning(descr(dat, "Species"))

          })


dat <- iris %>% mutate(cat= factor(rep(c("a", "b", "c"), 50)) )
test_that("No test is calculated if a test is requested that is not implemented",
          {
            expect_error(descr(dat, "Species", test_options = list(exact=TRUE, paired=TRUE, indices=c(1:50, 1:50, 1:50)),
                               format_options=list(print_Total=FALSE)), NA)
            expect_error(descr(dat, "Species", test_options = list(exact=TRUE, paired=FALSE),
                               format_options=list(print_Total=FALSE)), NA)
                         expect_error(descr(dat, "Species", test_options = list(exact=FALSE, paired=TRUE, indices=c(1:50, 1:50, 1:50)),
                                            format_options=list(print_Total=FALSE)), NA)
          }
          )




dat <- tibble(a=c("a", ""), b=factor(c("a", "")))
dat2 <- tibble(a=c("a", "(empty)"), b=factor(c("a", "")))
test_that("Empty strings are handled correctly",
          {
            expect_warning(expect_warning(descr(dat)))
            expect_error(descr(dat2, format_options=list(replace_empty_string_with_NA=FALSE)) )
            expect_warning(expect_warning(descr(dat2)))
            expect_warning(expect_warning(expect_warning(descr(dat, format_options=list(replace_empty_string_with_NA=FALSE)))))
          }
)


set.seed(123)
# Copied from https://stackoverflow.com/a/21502397
dat <- data.frame(sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 100))
test_that("Date variables throw warnings.",{
          expect_warning(expect_warning(descr(dat)))
})

date1 <- c(as.Date("20.01.2020", tryFormats = "%d.%m.%Y"), as.Date("21.01.2020", tryFormats = "%d.%m.%Y"))
date2 <- c(as.Date("23.01.2020", tryFormats = "%d.%m.%Y"), as.Date("17.01.2020", tryFormats = "%d.%m.%Y"))
datediff <- date2 - date1
dat <- tibble(date1 = date1, date2 = date2, datediff = datediff)

test_that(
  "date and difftime is converted correctly",
  {
    expect_warning(expect_warning(expect_warning(expect_warning(descr(dat)))))
  }
)






