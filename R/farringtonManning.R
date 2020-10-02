#' @title Farrington-Manning test for rate difference
#'
#' @description The Farrington-Manning test for rate differences can be used to
#'  compare the rate difference of successes between two groups to a preset value.
#'  It uses an explicit formula for the standard deviation of the test statistic under
#'  the null hypothesis [1].
#'
#' @details The Farrington-Maning test for rate differences test the null hypothesis
#'  of \deqn{H_{0}: p_{1} - p_{2} = \delta}{H[0]: p[1] - p[2] = \delta} for the "two.sided" alternative
#'  (or \eqn{\geq}{\ge} for the "greater" respectively \eqn{\leq}{\le} for the "less" alternative).
#'  This formulation allows to specify non-inferiority and superiority
#'  test in a consistent manner:
#'  \describe{
#'      \item{non-inferiority}{for delta < 0 and alternative == "greater" the null hypothesis
#'      reads \eqn{H_{0}: p_{1} - p_{2} \geq \delta}{H[0]: p[1] - p[2] \ge \delta} and
#'      consequently rejection allows concluding that \eqn{p_1 \geq p_2 + \delta}{p[1] \ge p[2] + \delta}
#'      i.e. that the rate of success in group one is at least the
#'      success rate in group two plus delta - as delta is negagtive this is equivalent to the success rate of group 1
#'      being at worst |delta| smaller than that of group 2.}
#'      \item{superiority}{for delta >= 0 and alternative == "greater" the null hypothesis
#'      reads \eqn{H_{0}: p_{1} - p_{2} \geq \delta}{H[0]: p[1] - p[2] \ge \delta} and
#'      consequently rejection allows concluding that \eqn{p_1 \geq p_2 + \delta}{p[1] \ge p[2] + \delta}
#'      i.e. that the rate of success in group one is at least delta greater than the
#'      success rate in group two.}
#' }
#' The confidence interval is always computed as two-sided, but with 1-2\eqn{\alpha} confidence level
#' in case of a one-sided hypthesis. This means that the lower or upper vound are valid one-sided
#' confidence bounds at level \eqn{\alpha} in this case.
#' The confidence interval is constructed by inverting the two-sided test directly.
#'
#' @param group1 a logical vector of data from group 1, where \code{TRUE} indicates a success
#' @param group2 a logical vector of data from group 2, where \code{TRUE} indicates a success
#' @param delta the rate difference under the null hypothesis
#' @param alternative character string indicating the alternative to use, either of
#'  "two.sided", "less", "greater"
#' @param alpha the significance level (acceptable error of the first kind),
#'  a two-sided confidence intnerval is returned with confidence level 1 - 2*alpha, such that
#'  the lower bound is a valid one sided confidence interval at the confidence level 1 - alpha.
#'
#' @return A list of class "htest" containing the following components:
#' \tabular{ll}{
#'  \code{statistic}:\tab the value of the Z-statistic\cr
#'  \code{parameter}:\tab delta, rate difference (group 1 - group 2) under the null hypothesis\cr
#'  \code{p.value}:\tab the p-value for the Farrington-Manning test\cr
#'  \code{null.value}:\tab rate difference (group 1 - group 2) under the null\cr
#'  \code{alternative}:\tab a character string indicating the alternative hypothesis\cr
#'  \code{method}:\tab a character string indicating the exact method employed\cr
#'  \code{data.name}:\tab a character string giving the names of the data used\cr
#'  \code{estimate}:\tab the estimated rate difference (maximum likelihood)\cr
#'  \code{conf.int}:\tab a confidence interval for the rate difference\cr
#'  \code{sample.size}:\tab the total sample size used for the test\cr
#' }
#'
#' @examples
#' x <- c(rep(T, 20), rep(F, 15))
#' y <- c(rep(T, 30), rep(F, 25))
#'
#' farrington.manning(x, y, -.3)
#'
#' @author Kevin Kunzmann
#'
#' @references [1] Farrington, Conor P., and Godfrey Manning. "Test statistics and sample size formulae for comparative binomial trials with null hypothesis of non-zero risk difference or non-unity relative risk." Statistics in medicine 9.12 (1990): 1447-1454.
#'
#' @keywords Farrington-Manning, rates, test
#'
#' @export
farrington.manning <- function(
  group1,
  group2,
  delta = 0, # note that the null is formulates in terms of -delta!
  alternative = "greater",
  alpha = 0.025
) {
  # remove na's
  if (any(is.na(group1)) | any(is.na(group2))) {
    warning("There are missing values in either of the groups, will be ignored.")
    group1 <- group1[!is.na(group1)]
    group2 <- group2[!is.na(group2)]
  }

  # check for logical input vectors
  if (!is.logical(group1) | !is.logical(group2)) {
    stop("Inputs group1 and group2 must be logical vectors!")
  }

  # check alternative
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop("alternative must be either two.sided, less or greater")
  }

  # check alpha range
  if (0 >= alpha | alpha >= 1) {
    stop("alpha must be in (0, 1)")
  }

  # check delta
  if (delta >= 1) {
    stop("A rate difference of < 1 is not possible.")
  }
  if (delta <= -1) {
    stop("A rate difference of > 1 is not possible.")
  }

  # construct results object
  res <- list()
  class(res) <- "htest"
  res$null.value  <- c(delta)
  names(res$null.value) <- c("rate difference (group 1 - group 2)")
  res$alternative <- alternative
  str <- "Farrington-Manning test for rate differences"
  if (alternative == "greater" & delta < 0) {
    str <- "Non-inferiority test for rates according to Farrington-Manning"
  }
  if (alternative == "greater" & delta >= 0) {
    str <- "Superiorty test for rates according to Farrington-Manning"
  }
  res$method      <- "Farrington-Manning test for non-inferiority of rates"
  res$data.name   <- sprintf("group 1: %s, group 2: %s", deparse(substitute(group1)), deparse(substitute(group2)))
  res$parameters  <- c(delta)
  names(res$parameters) <- c("noninferiority margin")

  # number of samples per group
  n1 <- length(group1)
  n2 <- length(group2)
  res$sample.size <- n1 + n2

  # compute maximum likelihood estimates
  p1_ML <- mean(group1)
  p2_ML <- mean(group2)

  # rate difference
  diff_ML <- p1_ML - p2_ML
  res$estimate <- c(diff_ML)
  names(res$estimate) <- c("rate difference (group 1 - group 2)")

  # standard deviation of the rate difference under the null hypothesis (risk difference = -delta)
  get_sd_diff_ML_null <- function(delta) {
    theta           <- n2/n1
    d               <- -p1_ML*delta*(1 + delta)
    c               <- delta^2 + delta*(2*p1_ML + theta + 1) + p1_ML + theta*p2_ML
    b               <- -(1 + theta + p1_ML + theta*p2_ML + delta*(theta + 2))
    a               <- 1 + theta
    v               <- b^3/(27*a^3) - b*c/(6*a^2) + d/(2*a)
    u               <- sign(v)*sqrt(b^2/(9*a^2) - c/(3*a))
    w               <- (pi + acos(v/u^3))/3
    p1_ML_null      <- 2*u*cos(w) - b/(3*a)
    p2_ML_null      <- p1_ML_null - delta
    sd_diff_ML_null <- sqrt(p1_ML_null*(1 - p1_ML_null)/n1 + p2_ML_null*(1 - p2_ML_null)/n2)
    return(sd_diff_ML_null)
  }
  sd_diff_ML_null <- get_sd_diff_ML_null(delta)

  # test statistic
  get_z <- function(delta) {
    z <- (diff_ML - delta)/get_sd_diff_ML_null(delta)
    return(z)
  }
  z <- get_z(delta)
  res$statistic <- z
  names(res$statistic) <- "Z-statistic"

  # p-value, probability of Z > < == z
  p_value_greater <- 1 - pnorm(z)
  p_value_less <- pnorm(z)
  p_value_two.sided <- 2*min(p_value_less, p_value_greater)
  if (alternative == "greater") {
    res$p.value <- p_value_greater
  }
  if (alternative == "less") {
    res$p.value <- p_value_less
  }
  if (alternative == "two.sided") {
    res$p.value <- p_value_two.sided
  }

    # confidence interval by inversion of two-sided test
  p_value_two.sided <- function(delta) {
    z <- get_z(delta)
    p_value_greater <- 1 - pnorm(z)
    p_value_less <- pnorm(z)
    2*min(p_value_less, p_value_greater)
  }

  alpha_mod <- ifelse(alternative == "two.sided", alpha, 2*alpha)
  ci_lo <- uniroot(
    function(delta) p_value_two.sided(delta) - alpha_mod, interval = c(-1+1e-6, res$estimate), tol = 1e-12
  )$root
  ci_hi <- uniroot(
    function(delta) p_value_two.sided(delta) - alpha_mod, interval = c(res$estimate, 1 - 1e-6), tol = 1e-12
  )$root

  # confidence interval
  res$conf.int <- c(ci_lo, ci_hi)
  attr(res$conf.int, "conf.level") <- 1 - alpha_mod

  return(res)
}
