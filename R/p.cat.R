#' p-value calculator for categorical variables
#'
#' Calculate the p-value for categorial variables.
#' The decision which test to use is equal to \code{\link{m.cat}}.
#' The p-value is calculated using one of the three tests:
#' Wilcoxon-Test, McNemar-Test, Chi-Squared-Test.
#'
#' @param x
#' Vector of the categorial variable.
#' @param group
#' Vector of the grouping variable.
#' @param paired
#' Logical. Is the categorial Variable paired?
#' @param is.ordered
#' Logical. Is the categorial Variable ordered?
#' @param correct.cat
#' Logical. Should correction be used in chi-sqared tests (see \code{\link{chisq.test}})
#' @param correct.wilcox
#' Logical. Should correction be used in wilcoxon tests (see \code{\link{wilcox.test}})
#' @param index
#' Optional. Label for the footnote.
#' The footnotes aren't produced in this function.
#' @param create
#' Which output document should be produced in the following step
#' (one of "pdf", "tex", "knitr", or "word").
#' Only usefull if \code{index} is not \code{NULL}.
#'
#' @details
#' Wilcoxon-Test: A Test for a comparison of 2 (in)dependent, ordered samples.
#' (see \code{\link{wilcox.test}}).
#' Kruskal_wallis-Test: A Test for a comparison of more than 2 (in)dependent,
#' ordered samples. (see \code{\link{kruskal.test}}).
#' McNemar Test: A Test for a comparison of 2 or more than 2 dependent,
#' not ordered samples. (see \code{\link{mcnemar.test}}).
#' Chi-Squared Test: A Test for a comparison of 2 or more than 2 independent,
#' not ordered samples. (see \code{\link{chisq.test}}).
#'
#' @return
#' The p-value with index which test is ussed is returned.
#'
#' @author
#' Lorenz Uhlmann, Csilla van Lunteren
#'
#' @examples
#' \dontrun{
#' p.cat(x=rep(1:5,20), group=rep(1:4,25))
#' }
#'
p.cat <- function(x, group, paired = F, is.ordered = F, correct.cat = F,
                  correct.wilcox = T, index = c(), create = "tex",
                  default.unordered.unpaired.test = "Chisq") {

  group <- droplevels(group);
  x <- droplevels(x);

  if (is.ordered) {
    if (length(levels(group)) == 2) {
      test.name <- "Wilcoxen"
      tl <- stats::wilcox.test(as.numeric(x) ~ group, paired = paired)
      pv <- tl$p.value
      test.value <- tl$statistic
    } else {
      test.name <- "Kruskal"
      tl <- stats::kruskal.test(x ~ group)
      pv <- tl$p.value
      test.value <- tl$statistic
    }
  } else {
    if (paired) {
      test.name <- "McNemar"
      tl <- stats::mcnemar.test(table(x, group), correct = correct.cat)
      pv <- tl$p.value
      test.value <- tl$statistic
    } else {
      if (default.unordered.unpaired.test == "Chisq"){
        test.name <- "Chisq"
        tl <- stats::chisq.test(x, group, correct = correct.cat)
        pv <- tl$p.value
        test.value <- tl$statistic
      }
      else if (default.unordered.unpaired.test == "Fisher_boschloo"){
        if ((nrow(table(x,group))!= 2) | (ncol(table(x,group))!= 2)){
          warning("Fisher_boschloo test not implemented for non-2x2 tables. Defaulting to Fisher_exact.")
          test.name <- "Fisher_exact"
          tl <- stats::fisher.test(x, group)
          pv <- tl$p.value
          test.value <- 0
        }
        else{
          test.name <- "Fisher_boschloo"
          tl <- Exact::exact.test(table(group,x), method="boschloo", to.plot=F)
          pv <- tl$p.value
          test.value <- tl$statistic
        }
      }
      else if (default.unordered.unpaired.test == "Fisher_exact"){
        test.name <- "Fisher_exact"
        tl <- stats::fisher.test(x, group)
        pv <- tl$p.value
        test.value <- 0
      }
    }
  }
  pform <- formatr(pv, 3, cl.z = T)
  if (!is.null(index)) {
    if (create == "word" | create == "R" | create== "archive") {
      pform <- paste(pform, index, sep = "")
    } else {
      if(grepl("<", pform)){
        pform <- paste("\\textless", gsub("<", "", pform), "$^", index, "$", sep = "")
      }
      else{
        pform <- paste(pform, "$^", index, "$", sep = "")
      }
    }
  }
  list(pv.formatted = pform, p.value = pv, test.value = test.value, test.name = test.name)
}
