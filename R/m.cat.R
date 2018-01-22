#' @name m.cat
#' @alias m.cat
#' @title
#' Decide which test to use for the p-value calculation by categorial variables.
#' @description
#' It choose the test which should use for the p-value calculation for categorial variables. The Wilcoxon-Test, McNemar-Test and the Chi-Squared-Test are possible.
#' @usage
#' m.cat(group, paired = F, is.ordered = F)
#' @param group
#' Vector of the grouping variable.
#' @param paired
#' Logical. Is the categorial Variable paired?
#' @param is.ordered
#' Logical. Is the categorial Variable ordered?
#' @return
#' The name of the test is returned.
#' @author
#' Lorenz Uhlmann, Csilla van Lunteren
#' @examples
#' \dontrun
#' m.cat(paired=T, is.ordered=T)
#' @keyword test by categorial
#' @export
m.cat <- function(group, paired = F, is.ordered = F) {
  if (is.ordered) {
    if (length(levels(group)) == 2) {
      pv <- "wilcox"
    } else {
      pv <- "kruskal"
    }
  } else {
    if (paired) {
      pv <- "mcnemar"
    } else {
      pv <- "chisq"
    }
  }
  pv
}
