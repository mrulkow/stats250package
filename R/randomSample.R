
#' Random Sample
#'
#' Collects a random sample from provided data
#'
#' @param data Variable of interest (dataset$variable)
#' @param n Sample size
#' @param bins Bins used in histogram
#' @param title Title used in histogram
#' @param xlab Label used on x-axis of histogram
#' @return A random sample of size n from the specified data
#' @export
#' @examples
#' randomSample(iris$Petal.Length, n = 100, bins = 30, title = "Histogram of Petal Length", xlab = "Petal Length (in cm)")

randomSample <- function(data, n, bins, title, xlab){

  random_sample <- as.data.frame(data[sample(1:length(data), n)])
  colnames(random_sample) <- "sampledata"

  ggplot(random_sample, aes(x = sampledata)) +

    geom_histogram(bins = bins,
                   color = "black",
                   fill = "grey") +

    labs(title = title,
         subtitle = paste("(for a random sample of", n, "observations)"),
         x = xlab,
         y = "Frequency")

}
