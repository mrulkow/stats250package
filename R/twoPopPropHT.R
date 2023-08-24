
#' Hypothesis Test for a Difference in Two Population Proportions
#'
#' Tests the hypotheses for a difference in two population proportions
#'
#' @param x1 Number of successes in sample 1
#' @param n1 Sample size of sample 1
#' @param x2 Number of successes in sample 2
#' @param n2 Sample size of sample 2
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less"
#' @return Hypothesis test information including the sample proportions, resulting test statistic, and corresponding p-value
#' @export
#' @examples
#' twoPopPropHT(x1 = 55, n1 = 100, x2 = 62, n2 = 100)
#' twoPopPropHT(x1 = 47, n1 = 112, x2 = 39, n2 = 102, alternative = "greater")

twoPopPropHT <- function(x1, n1, x2, n2, alternative = "two.sided"){

  # Calculates the sample proportions
  phat1 <- x1 / n1
  phat2 <- x2 / n2

  # Calculates the estimate of the common population proportion
  phat <- ((n1 * phat1) + (n2 * phat2)) / (n1 + n2)

  # Calculates the null standard error
  null.se <- sqrt((phat * (1 - phat)) * ((1 / n1) + (1 / n2)))

  # Calculates the z test statistic
  z.test.stat <- (phat1 - phat2) / null.se

  # Calculates the p-value (based on the alternative hypothesis provided - default is two-sided)
  if (alternative == "greater") {p.value <- pnorm(z.test.stat, lower.tail = FALSE)
  } else if (alternative == "less") {p.value <- pnorm(z.test.stat, lower.tail = TRUE)
  } else if (alternative == "two.sided") {p.value <- 2 * pnorm(abs(z.test.stat), lower.tail = FALSE)
  }

  # Returns list of helpful values
  return(list("phat1" = phat1, "phat2" = phat2,
              "z.test.stat" = z.test.stat, "p.value" = p.value))
}
