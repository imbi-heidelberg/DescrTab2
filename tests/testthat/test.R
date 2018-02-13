library(DescrTab)

attach(infert)
infert$education<-as.factor(education)
infert$induced<-as.factor(induced)
infert$case<-as.factor(case)
infert$spontaneous<-as.factor(spontaneous)


for (index in c(T,F)) {
for (nonparametric in c(T,F)) {
for (groupsize in c(T,F)) {
for (group.miss in c(T,F)) {
for (percent.vertical in c(T,F)) {
  file <- "Bibliotheken/Dokumente.pdf"
  test_that("pdf", {
    expect_that(des.print(dat = infert, group = "case", create = "pdf", file = file, index = index, fsize = 11, paired = F, nonparametric = nonparametric,
                         var.equal = T, correct.cat = F, correct.wilcox = T, t.log = c(), which.col = c("groups", "total", "p-values"),
                         groupsize = groupsize, n.or.miss = c("n", "miss", "miss.cat"), group.miss = group.miss, percent.vertical = percent.vertical,
                         var.names = c("education", "age", "parity","induced", "spontaneous", "stratum", "pooled.stratum"), data.names = T,
                         caption = c("Group 1", "Group 2"), tab.caption = "Statistics", landscape = F, pos.pagebr = NULL, label = NULL, digits.m = 1,
                         digits.sd = 2, digits.qu = c(), digits.minmax = 1, digits.p = 1, silent = T),prints_text("Descriptive statistics table successfully created."))
  })
  unlink("a.pdf")

  file <- "Bibliotheken/Dokumente.docx"
  test_that("Tests", {
    expect_that(des.print(dat = infert, group = "case", create = "word", file = file, index = index, fsize = 11, paired = F, nonparametric = nonparametric,
                          var.equal = T, correct.cat = F, correct.wilcox = T, t.log = c(), which.col = c("groups", "total", "p-values"),
                          groupsize = groupsize, n.or.miss = c("n", "miss", "miss.cat"), group.miss = group.miss, percent.vertical = percent.vertical,
                          var.names = c("education", "age", "parity","induced", "spontaneous", "stratum", "pooled.stratum"), data.names = T,
                          caption = c("Group 1", "Group 2"), tab.caption = "Statistics", landscape = F, pos.pagebr = NULL, label = NULL, digits.m = 1,
                          digits.sd = 2, digits.qu = c(), digits.minmax = 1, digits.p = 1, silent = T),prints_text("Descriptive statistics table successfully created."))
  })
}}}}}
