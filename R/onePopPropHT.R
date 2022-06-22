
#' Hypothesis Test for a Population Proportion Function
#'
#' This function runs a hypothesis test for a population proportion.
#' 
#' @param x Number of successes
#' @param n Sample size
#' @param p0 Null population proportion (from H0)
#' @param alt A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less"
#' @return Hypothesis test information including the sample proportion, resulting test statistic, and corresponding p-value
#' @export
#' @examples
#' onePopPropHT(x = 55, n = 100, p0 = 0.50)
#' onePopPropHT(x = 47, n = 112, p0 = 0.4, alt = "greater")

onePopPropHT <- function(x, n, p0, alt = "two.sided"){
  
  # Calculates the sample proportion and z test statistic
  phat <- x / n
  
  # Calculates the null standard deviation
  null.sd <- sqrt((p0 * (1 - p0)) / n)
  
  # Calculates the z test statistic
  z.test.stat <- (phat - p0) / null.sd
  
  # Calculates the p-value (based on the alternative hypothesis provided - default is two-sided)
  if (alt == "greater") {p.value <- pnorm(z.test.stat, lower.tail = FALSE)
  } else if (alt == "less") {p.value <- pnorm(z.test.stat, lower.tail = TRUE)
  } else if (alt == "two.sided") {p.value <- 2 * pnorm(abs(z.test.stat), lower.tail = FALSE)
  }
  
  # Returns list of helpful values
  return(list("phat" = phat, "null.sd" = null.sd, "test.stat" = z.test.stat, "p.value" = p.value))
}

