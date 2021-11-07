library(magrittr, quietly = TRUE, warn.conflicts = FALSE)


x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
dat <- tibble(diff = x - y)


test_that("wilcox.test_1_sample does not produce error",
          {
            expect_error(descr(dat, test_options = c(nonparametric = TRUE)) %>% print(silent =
                                                                                        TRUE), NA)
          })

test_that("wilcox.test_1_sample does not produce error categorical",
          {
            expect_error(descr(
              dat %>% mutate(diff = ordered(diff)),
              test_options = c(nonparametric = TRUE)
            ), NA)
          })


verify_output(ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/wilcox.test_1_sample.txt"),
              descr(dat, test_options = c(nonparametric = TRUE)) %>% print())


x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
group <- c(rep("Trt", length(x)), rep("Ctrl", length(y)))
dat_wilcox.test_2_sample <- tibble(var = c(x, y), group = group)
dat_wilcox.test_2_sample_paired <-
  tibble(var = c(x, y, y), group = c(rep("Trt", 10), rep("Ctrl", 10)) )
dat_wilcox.test_2_sample_paired2 <-
  tibble(var = c(x, y, y), group = c(rep("Trt", 10), rep("Ctrl", 10)), ID = factor(rep(1:10, 2)) )

test_that("wilcox.test_2_sample",
          {
          expect_error(
            descr(
              dat_wilcox.test_2_sample,
              "group",
              test_options = c(nonparametric = TRUE)
            ) %>% print(silent = TRUE),
            NA
          )})

test_that("wilcox.test_2_sample categorical",
          {
          expect_error(
            descr(
              dat_wilcox.test_2_sample %>% mutate(var = ordered(var)),
              "group",
              test_options = c(nonparametric = TRUE)
            ) %>% print(silent = TRUE),
            NA
          )})

test_that("wilcox.test_2_sample paired with ID in options",
          {
          expect_error(
            descr(
              dat_wilcox.test_2_sample_paired,
              "group",
              test_options = list(
                nonparametric = TRUE,
                paired = TRUE,
                indices = rep(1:10, 2)
              ),
              format_options = list(print_Total = FALSE)
            ),
            NA
          )})

test_that("wilcox.test_2_sample paired with ID in dataset",
          {
            expect_error(
              descr(
                dat_wilcox.test_2_sample_paired2,
                "group",
                test_options = list(
                  nonparametric = TRUE,
                  paired = TRUE,
                  indices = "ID"
                ),
                format_options = list(print_Total = FALSE)
              ),
              NA
            )})

test_that("wilcox.test_2_sample paired errors if you forget to specify indices",
          {
            expect_message(
              descr(
                dat_wilcox.test_2_sample_paired,
                "group",
                test_options = list(nonparametric = TRUE,
                                    paired = TRUE),
                format_options=list(print_Total=FALSE)
              )
            )


            expect_message(
              descr(
                dat_wilcox.test_2_sample_paired %>%  mutate(var = ordered(var)),
                "group",
                test_options = list(nonparametric = TRUE,
                                    paired = TRUE),
                format_options=list(print_Total=FALSE)
              )
            )


          })

test_that("wilcox.test_2_sample paired works with missings",
          {
          expect_warning(
            descr(
              dat_wilcox.test_2_sample_paired %>% mutate(var = if_else(row_number()==1, NA_real_, var)),
              "group",
              test_options = list(
                nonparametric = TRUE,
                paired = TRUE,
                indices = rep(1:10, 2)
              ),
              format_options = list(print_Total=FALSE)
            )
          )})

test_that("wilcox.test_2_sample paired categorical",
          {
            expect_error(
              descr(
                dat_wilcox.test_2_sample_paired %>% mutate(var = ordered(var)),
                "group",
                test_options = list(
                  nonparametric = TRUE,
                  paired = TRUE,
                  indices = rep(1:10, 2)
                ),
                format_options = list(print_Total = FALSE)
              ) %>% print(silent = TRUE),
              NA
            )
          })

verify_output(
  ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/wilcox.test_2_sample.txt"),
  descr(
    dat_wilcox.test_2_sample,
    "group",
    test_options = c(nonparametric = TRUE)
  ) %>% print()
)


x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
group <-
  c(rep("Trt", length(x)),
    rep("Ctrl", length(y)),
    rep("Placebo", length(z)))
dat_kruskal.test <- tibble(var = c(x, y, z), group = group)

test_that("kruskal.test works",
          {
            expect_error(descr(
              dat_kruskal.test,
              "group",
              test_options = c(nonparametric = TRUE)
            ) %>%
              print(silent = TRUE),
            NA)
          })

test_that("kruskal.test works categorical",
          {
            expect_error(
              descr(
                dat_kruskal.test %>% mutate(var = ordered(var)),
                "group",
                test_options = c(nonparametric = TRUE)
              ) %>%
                print(silent = TRUE),
              NA
            )
          })

verify_output(
  ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/kruskal.test.txt"),
  descr(dat_kruskal.test, "group", test_options = c(nonparametric = TRUE)) %>% print()
)



## ----friedman.test, results='asis'------------------------------------------------------------------

RoundingTimes <-
  matrix(
    c(
      5.459,5.509,5.509,5.859,5.709,5.759,5.209,5.609,5.509,5.559,5.509,5.409,5.909,5.859,5.709,5.459,5.559,5.609,5.409,5.409,
      5.359,5.459,5.509,5.359,5.259,5.159,5.009,5.859,5.809,5.759,5.259,5.259,5.159,5.609,5.559,5.409,5.609,5.359,5.459,5.059,
      5.009,4.959,5.509,5.509,5.409,5.459,5.559,5.509,5.559,5.559,5.359,5.459,5.509,5.559,5.509,5.459,5.259,5.659,5.609,5.409,
      5.709,5.659,5.559,6.309,6.309,6.259
    ),
    nrow = 22,
    byrow = TRUE,
    dimnames = list(1:22,
                    c("Round Out", "Narrow Angle", "Wide Angle"))
  )

idx <- rep(1:22, 3)
dat <-
  tibble(
    var = c(RoundingTimes[, 1], RoundingTimes[, 2], RoundingTimes[, 3]),
    group = c(
      rep("Round Out", 22),
      rep("Narrow Angle", 22),
      rep("Wide Angle", 22)
    )
  )



test_that("friedman.test works",
          {
            expect_error(
              descr(
                dat,
                "group",
                test_options = list(
                  nonparametric = TRUE,
                  indices = idx,
                  paired = T
                ),
                format_options = list(print_Total = FALSE)
              ) %>%
                print(silent = TRUE),
              NA
            )
          })


test_that("friedman.test works",
          {
          expect_error(
            descr(
              dat %>% mutate(var = ordered(var)),
              "group",
              test_options = list(
                nonparametric = TRUE,
                indices = idx,
                paired = T
              ),
              format_options=list(print_Total=FALSE)
            ),
            NA
          )})


verify_output( ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/friedman.test.txt"),
              descr(
                dat,
                "group",
                test_options = list(
                  nonparametric = TRUE,
                  indices = idx,
                  paired = T
                )
              ) %>% print())




## ----CochranQTest, results='asis'-------------------------------------------------------------------

d.frm <- DescTools::Untable(xtabs(c(6, 2, 2, 6, 16, 4, 4, 6) ~ .,
                                  expand.grid(rep(list(
                                    c("F", "U")
                                  ), times = 3))),
                            colnames = LETTERS[1:3])

# rearrange to long shape
d.long <- reshape(
  d.frm,
  varying = 1:3,
  times = names(d.frm)[c(1:3)],
  v.names = "resp",
  direction = "long"
)
idx <- d.long$id
dat <-
  d.long[, 1:2] %>% mutate(time = as.character(time), resp = as.character(resp))


test_that("CochranQTest works",
          {
          expect_error(descr(
            dat, "time", test_options = list(indices = idx, paired = TRUE),
            format_options=list(print_Total=FALSE)
          ),
          NA)})


verify_output(ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/CochraneQTest.txt"),
              descr(dat, "time", test_options = list(indices = idx, paired = TRUE)) %>% print())




## ----mcnemar.test, results='asis'-------------------------------------------------------------------
dat <-
  tibble::tibble(
    var = c(
      rep("Approve", 794),
      rep("Approve", 150),
      rep("Disapprove", 86),
      rep("Disapprove", 570),
      rep("Approve", 794),
      rep("Disapprove", 150),
      rep("Approve", 86),
      rep("Disapprove", 570)
    ),
    group = c(rep("first", 1600), rep("second", 1600))
  )

test_that("mcnemar.test works",
          {
          expect_message(expect_warning(descr(
            dat, "group", test_options = list(paired = TRUE, indices = c(1:1600, 1:1600))
          )))})

test_that("exact2x2 mcnemar test works",
          {
          expect_error(descr(
            dat, "group", test_options = list(
              paired = TRUE,
              exact = TRUE,
              indices = c(1:1600, 1:1600)
            ),
            format_options = list(print_Total = FALSE)
          ),
          NA)})

test_that("exact2x2 mcnemar test errors if you forget to specify indices",
          {
          expect_message(descr(
            dat, "group", test_options = list(
              paired = TRUE,
              exact = T
            ),
            format_options = list(print_Total = FALSE)
          ))})



verify_output(
  ifelse(
    isTRUE(write_in_tmpfile_for_cran()),
    tempfile(),
    "../console/mcnemar.test.txt"
  ),
  descr(dat, "group", test_options = list(
    paired = TRUE, indices = c(1:1600, 1:1600)
  )) %>% print()
)

verify_output(ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/exact_mcnemar.test.txt"),
              descr(
                dat, "group", test_options = list(
                  paired = TRUE,
                  exact = TRUE,
                  indices = c(1:1600, 1:1600)
                )
              ) %>% print()
)



## ----chisq.test, results='asis'---------------------------------------------------------------------

dat <-
  tibble(gender = c(rep("F", sum(c(
    762, 327, 468
  ))), rep("M", sum(c(
    484, 239, 477
  )))),
  party = c(
    rep("Democrat", 762),
    rep("Independent", 327),
    rep("Republican", 468),
    rep("Democrat", 484),
    rep("Independent", 239),
    rep("Republican", 477)
  ))



test_that("chisq.test 1 sample test works",
          {
          expect_error(descr(dat) %>%
                         print(silent = TRUE),
                       NA)})

test_that("chisq.test more sample test works",
          {
          expect_error(descr(dat, "gender"),
                       NA)})


verify_output(
  ifelse(
    isTRUE(write_in_tmpfile_for_cran()),
    tempfile(),
    "../console/1_sample_chisq.test.txt"
  ),
  descr(dat)
  %>% print()
)

verify_output(
  ifelse(
    isTRUE(write_in_tmpfile_for_cran()),
    tempfile(),
    "../console/more_sample_chisq.test.txt"
  ),
  descr(dat, "gender") %>% print()
)


## ----t.test, results='asis'-------------------------------------------------------------------------

dat <- sleep[, c("extra", "group")]


test_that("t.test 1 sample test works",
          {
            expect_error(descr(dat[, "extra"]) %>%
                           print(silent = TRUE),
                         NA)
          })

test_that("t.test 1 sample works",
          {
            expect_error(descr(
              dat[, "extra", drop = F] %>% mutate(extra = factor(extra)),
              test_options = list(test_override = "Student's one-sample t-test")
            ) %>%
              print(silent = TRUE),
            NA)
          })

test_that("t.test 2 sample works",
          {
            expect_error(descr(dat, "group"),
                         NA)
          })

test_that("t.test 2 sample works if specifically requrested",
          {
            expect_error(descr(dat, "group", var_options = list(
              extra = list(test_override = "Welch's two-sample t-test")
            )) %>%
              print(silent = TRUE),
            NA)
          })

test_that("t.test 2 sample for factor variables works",
          {
            expect_error(descr(
              dat %>% mutate(extra = factor(extra)),
              "group",
              test_options = list(
                paired = TRUE,
                indices = rep(1:10, 2),
                test_override  = "Welch's two-sample t-test"
              ),
              format_options = list(print_Total = FALSE)
            ),
            NA)
          })



test_that("t.test paired 2 sample test works",
          {
            expect_error(descr(
              dat,
              "group",
              test_options = list(
                paired = TRUE,
                indices = rep(1:10, 2)
              ),
              format_options = list(print_Total = FALSE)
            ), NA)
          })

test_that("t.test paired 2 sample test works",
          {
          expect_error(
            descr(
              dat %>% mutate(indices = rep(1:10, 2)),
              "group",
              test_options = list(paired = TRUE, indices = "indices"),
              format_options = list(print_Total = FALSE)
            ),
            NA
          )})


test_that("t.test paired 2 sample test works with missings",
          {
          expect_warning(
            descr(
              dat %>% mutate(indices = rep(1:10, 2)) %>% mutate(extra = if_else(row_number()==1, NA_real_, extra )),
              "group",
              test_options = list(paired = TRUE, indices = "indices"),
              format_options = list(print_Total = FALSE)
            )
          )})



test_that(
  "t.test paired 2 sample test for factor variables works",
  {
  expect_error(
    descr(
      dat %>% mutate(extra = factor(extra)),
      "group",
      test_options = list(
        paired = TRUE,
        indices = rep(1:10, 2),
        test_override  = "Student's paired t-test"
      ),
      format_options = list(print_Total = FALSE)
    ),
    NA
  )}
)

verify_output(ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/1_sample_t.test.txt"),
              descr(dat)
              %>% print())

verify_output(ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/2_sample_t.test.txt"),
              descr(dat, "group")
              %>% print())

verify_output(
  ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/1_samplepaired_t.test.txt"),
  descr(dat, "group", test_options = list(
    paired = TRUE, indices = rep(1:10, 2)
  ))
  %>% print()
)




## ----aov, results='asis'----------------------------------------------------------------------------

dat <- data.frame(
  y = npk$yield,
  P = ordered(gl(3, 24)),
  N = ordered(gl(3, 1, 24))
)



test_that("f.test test works",
          {
            expect_error(descr(dat[, c("y", "P")], "P"),
                         NA)
          })


test_that("f.test test works",
          {
          expect_error(
            descr(
              dat[, c("y", "P")] %>% mutate(y = factor(y)) ,
              "P",
              test_options = list(test_override = "F-test (ANOVA)")
            ) ,
            NA
          )})


verify_output(ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/f.test.txt"),
              descr(dat[, c("y", "P")], "P")
              %>% print())



## ----mixed, results='asis'--------------------------------------------------------------------------

dat <- nlme::Orthodont
dat2 <- nlme::Orthodont[1:64,]
dat2$Sex <- "Divers"
dat2$distance <-
  dat2$distance + c(rep(0.1 * c(1, 4, 3, 2), 10), 0.1 * rep(c(0.4, 2, 1.5, 2.3), 6))
dat2$Subject <- str_replace_all(dat2$Subject, "M", "D")
dat <- bind_rows(dat, dat2)
dat <- as_tibble(dat)


test_that("mixed_model test works",
          {
          expect_error(descr(
            dat[, c("Sex", "distance")],
            "Sex",
            test_options = list(paired = TRUE, indices =
                                  dat$Subject),
            format_options = list(print_Total = FALSE)
          ),
          NA)})

test_that("mixed_model test works",
          {
          expect_error(
            descr(
              dat[, c("Sex", "distance")] %>% mutate(distance = factor(distance)),
              "Sex",
              test_options = list(
                paired = TRUE,
                indices =
                  dat$Subject,
                test_override = "Mixed model ANOVA"
              ),
              format_options = list(print_Total = FALSE)
            ),
            NA
          )})



verify_output(ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/mixed_model.txt"),
              descr(dat[, c("Sex", "distance")], "Sex", test_options = list(
                paired = TRUE, indices =
                  dat$Subject
              )) %>% print())


dat <-
  tibble(
    gender = factor(
      c(
        "M","M","M","M","M","M","F","F","F","F","F","M","M","M","M","M","M","F","F","F","F","F","M","M","M","M",
        "M","M","F","F","F","F","F","M","M","M","M","M","M","F","F","F","F","F"
      )
    ),
    party = factor(
      c(
        "A","A","B","B","B","B","A","A","A","B","B","A","A","B","B","B","B","A","A","A","B","B","A","A","B","B",
        "B","B","A","A","A","B","B","A","A","B","B","B","B","A","A","A","B","B"  )
    )
  )

## boschloo

test_that("boschloo test works",
          {
          expect_error(descr(dat, "gender", test_options = c(exact = TRUE)) %>%
                         print(silent = TRUE),
                       NA)})
verify_output(ifelse(isTRUE(write_in_tmpfile_for_cran()), tempfile(), "../console/boschloo.txt"),
              descr(dat, "gender", test_options = c(exact = TRUE)) %>% print())


## test edge cases

test_that("tests are skipped if variables do not contain enough observations",
          {
            expect_warning(descr(data.frame(a = 1)))
            expect_warning(descr(data.frame(a = "a")))
          })


custom_ttest <- list(
  name = "custom t-test",
  abbreviation = "custom",
  p = function(var) {
    return(t.test(var)$p.value)
  }
)
custom_ttest2 <- list(
  name = "custom t-test",
  abbreviation = "custom",
  p = function(var, group) {
    return(t.test(var ~ group, data.frame(var = var, group = group))$p.value)
  }
)

test_that("Custom tests work", {
  expect_error(descr(iris %>% select(-Species), test_options = list(test_override = custom_ttest)), NA)

  expect_error(descr(iris %>% mutate(Species = fct_collapse(Species, setosa = c("setosa", "versicolor"))),
    "Species",
    var_options = list(Sepal.Length = list(test_options = list(test_override = custom_ttest2)))
  ), NA)
})








