
#' Simulation of the Sampling Distribution of the Sample Mean
#'
#' This function simulates the sampling distribution of the sample mean
#'
#' @param data Variable of interest (dataset$variable)
#' @param mu Instead of providing data, the population mean (mu) and population standard deviation (sigma) can be specified for a normal distribution
#' @param sigma Instead of providing data, the population mean (mu) and population standard deviation (sigma) can be specified for a normal distribution
#' @param n Sample size
#' @param reps Number of repetitions (default is 500)
#' @param zoom Logical argument (TRUE or FALSE) for determining the plot window
#' @param xlim Manually set the x-axis bounds using a vector (default is NA)
#' @return A simulated sampling distribution of the sample mean
#' @export
#' @examples
#' simMeans(data = trees$Height, n = 20)
#' simMeans(mu = 100, sigma = 10, n = 50, reps = 1000)

simMeans <- function(data, mu = NA, sigma = NA, n, reps = 500, zoom = FALSE, xlim = NA){

  if(is.na(mu)) {

    lb <- quantile(data, 0.05)
    ub <- quantile(data, 0.95)

    sample_means <- rep(0, reps)

    for(i in 1:reps) {
      randomSample <- data[sample(1:length(data), n, replace = FALSE)]
      sample_means[i] <- mean(randomSample)
    }

  }
  else {

    sample_means <- rep(0, reps)

    lb <- qnorm(p = 0.05, mean = mu, sd = sigma)
    ub <- qnorm(p = 0.95, mean = mu, sd = sigma)

    for(i in 1:reps) {
      randomSample <- rnorm(n, mean = mu, sd = sigma)
      sample_means[i] <- mean(randomSample)
    }

  }


  ggplot() +

    geom_dots(aes(sample_means),
              smooth = if(reps >= 100) {"unbounded"} else {"none"},
              layout = "hex",
              stackratio = 0.9,
              #scale = if(reps < 100) {0.5} else if (reps < 250) {0.75} else {1},
              binwidth = unit(c(1, Inf), "mm"),
              overflow = "compress",
              col = "grey20",
              alpha = 0.5) +

    scale_y_continuous(breaks = NULL, labels = NULL) +

    scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                       limits = if(is.na(xlim[1]) == FALSE) {xlim}
                       else if(zoom == TRUE) {c(min(sample_means) - (diff(range(sample_means))/10),
                                                max(sample_means) + (diff(range(sample_means))/10))}
                       else {c(lb, ub)}) +

    labs(title = bquote(paste("Simulated Sampling Distribution of ", bar(X))),
         subtitle = paste("(from ", reps, " random samples of size n = ", n, ")", sep = ""),
         x = NULL,
         y = NULL) +

    theme_classic() +

    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 16),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 12),
          panel.grid.major.x = element_line(color = "grey80",
                                            linetype = "dashed"),
          axis.line.y = element_blank(),
          legend.position = "none")

}
