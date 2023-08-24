
#' T Multiplier
#'
#' Computes the t multiplier for the specified confidence level
#'
#' @param confidence Desired confidence level
#' @param df Degrees of freedom
#' @return The t multiplier for the specified confidence level
#' @export
#' @examples
#' tMultiplier(confidence = 0.95, df = 35)

tMultiplier <- function(confidence, df){

  return(round(qt((1 - confidence) / 2, df = df, lower.tail = FALSE), 3))

}
