#' @title Minimum and maximum value of continous variables
#'
#' @description
#' Compute the minimum and maximum of continous variables.
#'
#' @usage
#' minmax(x, k = 1)
#'
#' @param x
#' Vector of the continous variable.
#' @param k
#' Number of decimal places.
#' If not specified, the number of decimal places is 1 or 2.
#'
#' @return
#' \code{"minimum" -- "maximum"} is returned.
#'
#' @author
#' Lorenz Uhlmann, Csilla van Lunteren
#'
#' @seealso
#' \code{\link{formatr}}
#'
#' @examples
#' \dontrun{
#' set.seed(12345)
#' x<-rnorm(100)
#'
#' minmax(x,k=2)
#' }
#'
minmax <- function(x, k = 1) {
  if (length(x) > 1){
    paste(formatr(min(x), k), " -- ", formatr(max(x), k), sep = "")
  }else{
    "-"
  }
}
