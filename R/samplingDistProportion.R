
#' Sampling Distribution for a Sample Proportion Function
#'
#' This function creates a sampling distribution for the sample proportion. This sampling distribution is visualized using 100 sample proportions taken from random samples of size n.
#' 
#' @param data Data set of interest (a data frame)
#' @param column Column number corresponding to the variable of interest from the data frame (default = 1)
#' @param n Sample size
#' @return A strip chart of 100 sample proportions (representing the sampling distribution of the sample proportion) generated from 100 random samples of size n
#' @export

samplingDistProportion <- function(data, column = 1, n){
  
  sample_proportions <- rep(0, 100)
  
  for(i in 1:100) {
    randomSample <- data[sample(1:nrow(data), n), column]
    sample_proportions[i] <- mean(randomSample == "yes")
  }
  
  stripchart(sample_proportions, method = "stack", pch = 20, at = 0,
             main = bquote(paste("Sampling Distribution of ", hat(p))),
             ylab = "Frequency",
             xlab = paste("100 Sample Proportions (from Samples of Size n = ", 
                          n, ")", sep =""))
  
}