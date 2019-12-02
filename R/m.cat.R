#' @title
#' Decide which test to use for the p-value calculation by categorial variables.
#'
#' @description
#' It choose the test which should use for the p-value calculation for categorial variables. The Wilcoxon-Test, McNemar-Test and the Chi-Squared-Test are possible.
#'
#' @usage
#' m.cat(group, paired = F, is.ordered = F)
#'
#' @param group
#' Vector of the grouping variable.
#' @param paired
#' Logical. Is the categorial Variable paired?
#' @param is.ordered
#' Logical. Is the categorial Variable ordered?
#'
#' @return
#' The name of the test is returned.
#'
#' @author
#' Lorenz Uhlmann, Csilla van Lunteren
#'
#' @examples
#' \dontrun{
#' m.cat(paired=T, is.ordered=T)
#' }
#'
m.cat <- function(x, group, paired = F, is.ordered = F, default.unordered.unpaired.test = "Chisq") {
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
      if (default.unordered.unpaired.test=="Chisq"){
        pv <- "chisq"
      }
      else if (default.unordered.unpaired.test=="Fisher_boschloo"){
        if ((nrow(table(x,group))!= 2) | (ncol(table(x,group))!= 2)){
          pv <- "Fisher_exact"
        }
        else{
          pv <- "Fisher_boschloo"
        }
      }
      else if (default.unordered.unpaired.test=="Fisher_exact"){
        pv <- "Fisher_exact"
      }
    }
  }
  pv
}
