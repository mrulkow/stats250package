
#' Confidence Interval for a Difference in Two Population Proportions
#'
#' Creates a confidence interval for a difference in two population proportions
#'
#' @param x1 Number of successes in sample 1
#' @param n1 Sample size of sample 1
#' @param x2 Number of successes in sample 2
#' @param n2 Sample size of sample 2
#' @param confidence Desired confidence level (default is 0.95)
#' @return The confidence interval for a difference in two population proportions
#' @export
#' @examples
#' twoPopPropCI(x1 = 55, n1 = 100, x2 = 62, n2 = 100)
#' twoPopPropCI(x1 = 47, n1 = 112, x2 = 39, n2 = 102, confidence = 0.90)

twoPopPropCI <- function(x1, n1, x2, n2, confidence = 0.95){

  # Calculates the sample proportions
  phat1 <- x1 / n1
  phat2 <- x2 / n2

  # Calculates the standard error
  se.phats <- sqrt(((phat1 * (1 - phat1)) / n1) + ((phat2 * (1 - phat2)) / n2))

  # Finds the z* multiplier
  z.multiplier <- qnorm((1 - confidence) / 2, lower.tail = FALSE)

  # Calculates the lower and upper bound of the interval
  lower.bound <- (phat1 - phat2) - z.multiplier*se.phats
  upper.bound <- (phat1 - phat2) + z.multiplier*se.phats

  # Returns a sentence with the results
  return(paste("The ", confidence*100, "% confidence interval is given as: (",
               round(lower.bound,4), ", ", round(upper.bound,4),").", sep = ""))
}
