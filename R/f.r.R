#' @title Format numbers
#' @description Rounds a number x to k decimal places.
#' @usage f.r(x, k)
#' @param x
#' Number, which should be rounded.
#' @param k
#' Number of decimal places.
#' @return
#' The rounded number is returned.
#' @author
#' Lorenz Uhlmann, Csilla van Lunteren
#' @examples
#' \dontrun{
#' set.seed(12345)
#' x<-rnorm(1)
#' k<-3
#' f.r(x,k)
#' }
#' @export
f.r <- function(x, k){
  format(round(x, k), nsmall = k)
}
