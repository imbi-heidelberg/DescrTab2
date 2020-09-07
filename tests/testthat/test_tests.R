context("Test statistical tests that produce p-values.")
options(print_format = "console")
library(magrittr)


x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
dat <- tibble(diff = x - y)


test_that("wilcox.test_1_sample does not produce error",
          expect_error(descr(dat, test_options = c(nonparametric = T)) %>% print(silent =
                                                                                   T), NA))

verify_output("../console/wilcox.test_1_sample.txt",
              descr(dat, test_options = c(nonparametric = T)) %>% print())


x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
group <- c(rep("Trt", length(x)), rep("Ctrl", length(y)))
dat_wilcox.test_2_sample <- tibble(var = c(x, y), group = group)

test_that("wilcox.test_2_sample",
          expect_error(
            descr(
              dat_wilcox.test_2_sample,
              "group",
              test_options = c(nonparametric = T)
            ) %>% print(silent = T),
            NA
          ))

verify_output(
  "../console/wilcox.test_2_sample.txt",
  descr(
    dat_wilcox.test_2_sample,
    "group",
    test_options = c(nonparametric = T)
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
          expect_error(
            descr(dat_kruskal.test, "group", test_options = c(nonparametric = T)) %>%
              print(silent = T),
            NA
          ))


verify_output(
  "../console/kruskal.test.txxt",
  descr(dat_kruskal.test, "group", test_options = c(nonparametric = T)) %>% print()
)



## ----friedman.test, results='asis'------------------------------------------------------------------

RoundingTimes <-
  matrix(
    c(
      5.40,
      5.50,
      5.55,
      5.85,
      5.70,
      5.75,
      5.20,
      5.60,
      5.50,
      5.55,
      5.50,
      5.40,
      5.90,
      5.85,
      5.70,
      5.45,
      5.55,
      5.60,
      5.40,
      5.40,
      5.35,
      5.45,
      5.50,
      5.35,
      5.25,
      5.15,
      5.00,
      5.85,
      5.80,
      5.70,
      5.25,
      5.20,
      5.10,
      5.65,
      5.55,
      5.45,
      5.60,
      5.35,
      5.45,
      5.05,
      5.00,
      4.95,
      5.50,
      5.50,
      5.40,
      5.45,
      5.55,
      5.50,
      5.55,
      5.55,
      5.35,
      5.45,
      5.50,
      5.55,
      5.50,
      5.45,
      5.25,
      5.65,
      5.60,
      5.40,
      5.70,
      5.65,
      5.55,
      6.30,
      6.30,
      6.25
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


descr(dat,
      "group",
      test_options = list(
        nonparametric = T,
        indices = idx,
        paired = T
      ))





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

descr(dat, "time", test_options = list(indices = idx, paired = T))





## ----mcnemar.test, results='asis'-------------------------------------------------------------------
dat <-
  tibble(var = c(
    rep("Approve", 794 + 150),
    rep("Disapprove", 86 + 570),
    rep("Approve", 794 + 86),
    rep("Disapprove", 150 + 570)
  ),
  group = c(rep("first", 1600), rep("second", 1600)))

descr(dat, "group", test_options = list(paired = T))
descr(dat, "group", test_options = list(paired = T, exact = T))
mcnemar.test(dat$var, dat$group)



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

descr(dat, "gender")
descr(dat)

chisq.test(dat$gender, dat$party)
chisq.test(table(dat$gender))
chisq.test(table(dat$party))





## ----t.test, results='asis'-------------------------------------------------------------------------

dat <- sleep[, c("extra", "group")]


descr(dat[, "extra"])
descr(dat, "group")
descr(dat, "group", test_options = list(paired = T, indices = rep(1:10, 2)))




## ----aov, results='asis'----------------------------------------------------------------------------

dat <- data.frame(
  y = c(449, 413, 326, 409, 358, 291, 341, 278, 312) / 12,
  P = ordered(gl(3, 3)),
  N = ordered(gl(3, 1, 9))
)

descr(dat[, c("y", "P")], "P")
descr(dat[, c("y", "N")], "N")




## ----mixed, results='asis'--------------------------------------------------------------------------

dat <- nlme::Orthodont
dat2 <- nlme::Orthodont[1:64,]
dat2$Sex <- "Divers"
dat2$Subject <- str_replace_all(dat2$Subject, "M", "D")
dat <- bind_rows(dat, dat2)
dat <- as_tibble(dat)

descr(dat[, c("Sex", "distance")], "Sex", test_options = list(paired = T, indices =
                                                                dat$Subject))




