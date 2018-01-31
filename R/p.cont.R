#' @title
#' p-value calculator for continous variables
#'
#' @description
#' Calculate the p-value for continous variables. The decision which test to use is equal to \code{\link{m.cont}}.
#' The p-value is calculated using one of the four tests: Wilcoxon-Test, t-Test, Kruskal-Test, Anova.
#'
#' @usage
#' p.cont(x, group, paired = F, is.ordered = F, nonparametric = F, t.log = F, var.equal = F, index = c(), create = "tex")
#'
#' @param x
#' Vector of the continous variable.
#' @param group
#' Vector of the grouping variable.
#' @param paired
#' Logical. Is the categorial Variable paired?
#' @param is.ordered
#' Logical. Is the categorial Variable ordered?
#' @param nonparametric
#' Logical. Should the continuous variable tested by using non-parametric methods.
#' @param t.log
#' Logical. Should be used the log of the original data.
#' @param var.equal
#' Logical. Should variances be assumed to be equal when applying t-tests?
#' @param index
#' Optional. Label for the footnote.
#' The footnotes aren't produced in this function.
#' @param create
#' Which output document should be produced in the following step (one of "pdf", "tex", "knitr", or "word").
#'
#' @details
#' Wilcoxon Test: A nonparametric Test for a comparison of 2 dependent samples. (see \code{\link{wilcox.test}}).
#' Mann-Whitney-U Test: A nonparametric Test for a comparison of 2 independent samples. (see \code{\link{wilcox.test}}).
#' t-Test: A parametric Test for a comparison of 2 (in)dependent samples. (see \code{\link{t.test}}).
#' Friedman-Test: A nonparametric Test for a comparison of more than 2 dependent samples. (see \code{\link{friedman.test}}).
#' Anova Type III: A parametric Test for a comparison of more than 2 dependent samples. (see \code{\link[car]{Anova}} with \code{}).
#' Kruskal-Wallis-Test: A nonparametric Test for a comparison of more than 2 independent samples. (see \code{\link{kruskal.test}}).
#' Anova: A parametric Test for a comparison of more than 2 independent samples. (see \code{\link{aov}}).
#'
#' @return
#' The p-value with index which test is ussed is returned.
#' author
#' Lorenz Uhlmann, Csilla van Lunteren
#'
#' @seealso
#' \link[nlme]{lme}
#' \link[car]{Anova}
#'
#' @examples
#' \dontrun{
#' p.cont(x=rnorm(100,0,1), group=rep(1:4,25))
#' }
#'
#' @import lme4
#' @import SparseM
#' @importFrom MatrixModels model.Matrix
#' @importFrom  nlme lme
#' @importFrom car Anova
#'
#' @export
#'
p.cont <- function(x, group, paired = F, is.ordered = F, nonparametric = F, t.log = F, var.equal = F,
                   index = c(), create = "tex") {
  if (length(levels(group)) == 2) {
    if (nonparametric) {
      pv <- wilcox.test(x ~ group, paired = paired)$p.value
    } else {
      if (t.log)
        x <- log(x)
      pv <- t.test(x ~ group, paired = paired, var.equal = var.equal)$p.value
    }
  } else {
    if (paired) {
      # Annahme: Beobachtungen stehen pro "Gruppe" jeweils in derselben Reihenfolge untereinander!
      x.ind <- rep(1:(length(x) / length(levels(group))), length(levels(group)))
      if (nonparametric) {
        x.ind <- rep(1:(length(x) / length(levels(group))), length(levels(group)))
        pv <- friedman.test(x ~ group | x.ind)$p.value
      } else {
        fit <- nlme::lme(x ~ group, random = ~ 1 | x.ind)
        pv <- car::Anova(fit, type = "III")[2, 3]
      }
    } else {
      if (nonparametric) {
        pv <- kruskal.test(x ~ group)$p.value
      } else {
        pv <- summary(aov(x ~ group))[[1]]$'Pr(>F)'[1]
      }
    }
  }
  pv <- formatr(pv, 3, cl.z = T)
  if (!is.null(index)) {
    if (create == "word" | create == "R") {
      pv <- paste(pv, index, sep = "")
    } else {
      pv <- paste(pv, "$^", index, "$", sep = "")
    }
  }
  pv
}
