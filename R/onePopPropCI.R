
#' Confidence Interval for One Population Proportion
#'
#' Creates a confidence interval for a population proportion
#'
#' @param x Number of successes
#' @param n Sample size
#' @param confidence Desired confidence level (default is 0.95)
#' @return The confidence interval for a population proportion
#' @export
#' @examples
#' onePopPropCI(x = 60, n = 100)
#' onePopPropCI(x = 47, n = 112, confidence = 0.90)

onePopPropCI <- function(x, n, confidence = 0.95){

  # Calculates the sample proportion
  phat <- x / n

  # Calculates the standard error
  se.phat <- sqrt((phat * (1 - phat)) / n)

  # Finds the z* multiplier
  z.multiplier <- qnorm((1 - confidence) / 2, lower.tail = FALSE)

  # Calculates the lower and upper bound of the interval
  lower.bound <- phat - z.multiplier*se.phat
  upper.bound <- phat + z.multiplier*se.phat

  # Returns a sentence with the results
  return(paste("The ", confidence*100, "% confidence interval is given as: (",
               round(lower.bound,4), ", ", round(upper.bound,4),").", sep = ""))
}
