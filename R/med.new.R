#' @title Median value for continous variables
#' @description
#' Compute the median of continous variables.
#' @usage
#' med.new(x, k = c())
#' @param x
#' Vector of the continous variable.
#' @param k
#' Number of decimal places.
#' If not specified, the number of decimal places is 1 or 2.
#' @return
#' The median is returned.
#' @author
#' Lorenz Uhlmann, Csilla van Lunteren
#' @seealso
#' \code{\link{formatr}}
#' @examples
#' \dontrun{
#' set.seed(12345)
#' x<-rnorm(100)
#'
#' med.new(x,k=c(3))
#' }
#' @export
med.new <- function(x, k = c()) {
  if (length(x)!=0){
    x.med <- median(x)
    if (is.null(k)) {
      if (as.integer(x.med / 0.25 / 2) == (x.med / 0.25 / 2) | as.integer(x.med / 0.25) != (x.med / 0.25)) {
        x.med <- round(x.med, 1)
      } else {
        x.med <- round(x.med, 2)
      }
    }else {
      x.med <- formatr(x.med, k)
    }
  }else{
    x.med <- "-"
  }
  x.med
}
