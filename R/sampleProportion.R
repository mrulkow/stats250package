
#' Sample Proportion
#'
#' Collects a random sample of n observations and computes the sample proportion based on a specified logical expression
#'
#' @param data Variable of interest (dataset$variable)
#' @param n Sample size
#' @param operator Symbol for the desired logical expression (>=, <, ==, etc.). Must be surrounded by backticks (``)
#' @param value Value used in the desired logical expression
#' @return A random sample of size n and its corresponding sample proportion
#' @export
#' @examples
#' sampleProportion(trees$Height, n = 10, operator = `>=`, value = 78)
#' sampleProportion(iris$Species, n = 30, operator = `==`, value = "setosa")

sampleProportion <- function(data, n, operator = `==`, value = "yes"){

  random_sample <- data[sample(1:length(data), n)]
  print(paste("Random Sample of", n, "Observations"))
  print(random_sample)
  print(paste("Sample Proportion:", mean(operator(random_sample, value))))

}
