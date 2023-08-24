
#' Sample Mean
#'
#' Collects a random sample of n observations and computes the sample mean
#'
#' @param data Variable of interest (dataset$variable)
#' @param n Sample size
#' @param plot Plots a histogram of the data when set to TRUE
#' @param title Supply a string for the title of the histogram
#' @param xlab Supply a string for the x-axis label
#' @param bins Number of bins used in the histogram (default is 30)
#' @return A random sample of size n, the corresponding sample mean, and an optional histogram of the data
#' @export
#' @examples
#' sampleMean(trees$Height, 10)
#' sampleMean(iris$Sepal.Length, n = 80, plot = TRUE, xlab = "Sepal Length (in cm)", bins = 15)

sampleMean <- function(data, n, plot = FALSE, title = "Histogram of a Random Sample", xlab = NULL, bins = NA){

  random_sample <- as.data.frame(data[sample(1:length(data), n)])
  colnames(random_sample) <- "sampledata"

  p <- ggplot(random_sample, aes(x = sampledata)) +

    geom_histogram(bins = if(is.na(bins)) {30} else {bins},
                   color = "black",
                   fill = "grey") +

    labs(title = title,
         subtitle = paste("Sample Mean = ", round(mean(random_sample$sampledata), 2), " (from a sample of size n = ", n, ")", sep = ""),
         x = xlab,
         y = "Frequency")

  if(plot == FALSE) {print(paste("Random Sample of", n, "Observations"))}
  if(plot == FALSE) {print(random_sample$sampledata)}
  if(plot == FALSE) {print(paste("Sample Mean:", mean(random_sample$sampledata)))}
  if(plot == TRUE) {print(p)}

}


