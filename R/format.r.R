#' @name formatr
#' @alias formatr
#' @title Format numbers
#' @description
#' Rounds a number x to k decimal places with a uniform representation (<0.001 for k=3) for all numbers that are not in the disabling area.
#' @usage formatr(x, k = 3, cl.z = F)
#' @param x
#' Number, which should be rounded.
#' @param k
#' Number of decimal places.
#' @param cl.z
#' Logical. Should a uniform representation be made for all numbers that are not in the display area?
#' @return
#' The rounded number is returned, maybe with uniform representation.
#' @author
#' Lorenz Uhlmann, Csilla van Lunteren
#' @seealso
#' \code{\link{f.r}}
#' @examples
#' \dontrun{
#' x<-c(0.1,0.01,0.001)
#' formatr(x, k=2, cl.z=T)}
#' @keyword Format numbers uniform
#' @export
formatr <- function(x, k = 3, cl.z = F) {
  val <- f.r(x, k)
  if (cl.z) {
    val[which(val == f.r(0, k))] <- paste("<", 0.1^k, sep = "")
  }
  val
}
