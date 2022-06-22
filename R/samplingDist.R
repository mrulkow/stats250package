
#' Sampling Distribution for a Sample Mean Function
#'
#' This function creates a sampling distribution for the sample mean. This sampling distribution is visualized using 10,000 sample means taken from random samples of size n.
#' 
#' @param data Data set of interest (a data frame)
#' @param column Column number corresponding to the variable of interest from the data frame (default = 1)
#' @param n Sample size
#' @return A histogram of 10,000 sample means (representing the sampling distribution of the sample mean) generated from 10,000 random samples of size n
#' @export

samplingDist <- function(data, column = 1, n){
  
  sample_means <- rep(0, 10000)
  
  for(i in 1:10000) {
    randomSample <- data[sample(1:nrow(data), n, replace = FALSE), column]
    sample_means[i] <- mean(randomSample)
  }
  
  x.min <- quantile(data[,column], 0.05)
  x.max <- quantile(data[,column], 0.95)
  
  hist(sample_means,
       main = bquote(paste("Sampling Distribution of ", bar(X))),
       xlab = paste("Possible Sample Means (from Samples of Size n = ", n, ")", sep =""),
       xlim = c(x.min, x.max))
  
}