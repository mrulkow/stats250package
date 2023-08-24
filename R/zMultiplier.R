
#' Z Multiplier
#'
#' Computes the z multiplier for the specified confidence level
#'
#' @param confidence Desired confidence level
#' @return The z multiplier for the specified confidence level
#' @export
#' @examples
#' zMultiplier(0.95)
#' zMultiplier(confidence = 0.99)

zMultiplier <- function(confidence){

  return(round(qnorm((1 - confidence) / 2, lower.tail = FALSE), 3))

}
