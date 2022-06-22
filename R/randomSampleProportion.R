
#' Random Sample Proportion Function
#'
#' This function calculates a sample proportion from a data set.
#' 
#' @param data Data set of interest (a data frame)
#' @param column Column number corresponding to the variable of interest from the data frame (default = 1)
#' @param n Sample size
#' @return The random sample of size n and the corresponding sample proportion
#' @export

randomSampleProportion <- function(data, column = 1, n){
  
  random_sample <- data[sample(1:nrow(data), n), column]
  print(paste("Variable:", colnames(data)[column]))
  print(paste("Generated Random Sample of Size", n))
  print(random_sample)
  print(paste("Sample Proportion:", mean(random_sample == "yes")))
  
}