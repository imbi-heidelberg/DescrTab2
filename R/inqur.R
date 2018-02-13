#' @title
#' Calculation of the first and third quantile
#'
#' @description
#' It calculate the first an third quantil of continuous variables.
#'
#' @usage
#' inqur(x, k = c())
#'
#' @param x
#' Vector of continuous cariables.
#' @param k
#' Optional. The number of decimal places.
#' If not specified, the number of decimal places is 1 or 2.
#'
#' @return
#' The first and third quantil is returned.
#'
#' @author
#' Lorenz Uhlmann, Csilla van Lunteren
#' seealso
#' \code{\link{formatr}}
#'
#' @examples
#' \dontrun{
#' set.seed(12345)
#' x<-rnorm(100)
#'
#' inqur(x, k = c(3))
#' }
#'
inqur <- function(x, k = c()) {
  if (length(x) > 1){
    x.q1 <- quantile(x, type = 7)[2]
    x.q3 <- quantile(x, type = 7)[4]
    if (is.null(k)) {
      if (as.integer(x.q1 / 0.25 / 2) == (x.q1 / 0.25 / 2) | (as.integer(x.q1 / 0.25) != (x.q1 / 0.25))) {
        x.q1 <- round(x.q1, 1)
      } else {
        x.q1 <- round(x.q1, 2)
      }
      if (as.integer(x.q3 / 0.25 / 2) == (x.q3 / 0.25 / 2) | (as.integer(x.q3 / 0.25) != (x.q3 / 0.25))) {
        x.q3 <- round(x.q3, 1)
      } else {
        x.q3 <- round(x.q3, 2)
      }
    }else {
      x.q1 <- formatr(x.q1, k)
      x.q3 <- formatr(x.q3, k)
    }
    paste(x.q1, " -- ", x.q3, sep = "")
  }else{
    paste("-")
  }

}
