#' @title
#' Decide which test to use for the p-value calculation by continous variables.
#' @description
#' It choose the test which should use for the p-value calculation for continous variables. The Wilcoxon-Test, t-Test, Firedman-Test, Anova Typ III, Kruskal-Test and a Anova are possible.
#' @usage
#' m.cont(group, paired = F, is.ordered = F, nonparametric = F, t.log = F)
#' @param group
#' Vector of the grouping variable.
#' @param paired
#' Logical. Is the categorial Variable paired?
#' @param is.ordered
#' Logical. Is the categorial Variable ordered?
#' @param nonparametric
#' Logical or vector of indices. If logical / vector of indices then all / only these continuous variables will be tested using non-parametric methods.
#' @param t.log
#' Vector of indices: The variables for which the log of the original data should be used when testing for a difference between the groups.
#' @return
#' The Name of the Test is returned.
#' @author
#' Lorenz Uhlmann, Csilla van Lunteren
#' @examples
#' \dontrun{
#' m.cont(group=rep(1:4,25))
#' }
#' @export
m.cont <- function(group, paired = F, is.ordered = F, nonparametric = F, t.log = F) {
  if (length(levels(group)) == 2) {
    if (nonparametric) {
      pv <- "Wilcox"
    } else {
      pv <- "t.test"
    }
  } else {
    if (paired) {
      if (nonparametric) {
        pv<- "Friedman"
      } else {
        pv <- "Anova Typ III"
      }
    } else {
      if (nonparametric) {
        pv <- "kruskal"
      } else {
        pv <- "aov"
      }
    }
  }
  pv
}

