
#' Instructor Demo Tool for the Sampling Distribution of the Sample Mean
#'
#' This function simulates the sampling distribution of the sample mean
#'
#' @param mu Instead of providing data, the population mean (mu) and population standard deviation (sigma) can be specified for a normal distribution
#' @param sigma Instead of providing data, the population mean (mu) and population standard deviation (sigma) can be specified for a normal distribution
#' @param n Sample size
#' @param reps Number of repetitions (default is 500)
#' @param show Number of sample mean examples to walk through (default is 5)
#' @param zoom Logical argument (TRUE or FALSE) for determining the plot window
#' @param xlim Manually set the x-axis bounds using a vector (default is NA)
#' @return Three plots: the distribution of the population, the distribution of the sample, and a simulated sampling distribution of the sample mean
#' @export
#' @examples
#' demoMeans(mu = 100, sigma = 10, n = 50)
#' demoMeans(mu = 100, sigma = 10, n = 20, show = 2, xlim = c(90, 110))

demoMeans <- function(mu, sigma, n, reps = 100, show = 5, zoom = TRUE, xlim = NA){

  sample_means <- rep(0, reps)
  random_sample <- vector(mode = "list", length = reps)

  for(i in 1:reps) {
    random_sample[[i]] <- rnorm(n = n, mean = mu, sd = sigma)
    sample_means[i] <- mean(random_sample[[i]])
  }

  suppressMessages({

    suppressWarnings({

      for(i in 1:show) {

        readline(prompt = "Press [enter] to continue")



        p1 <- ggplot() +

          stat_function(fun = dnorm,
                        args = list(mean = mu, sd = sigma),
                        lwd = 1.2) +

          scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                             limits = c(mu - 4*sigma, mu + 4*sigma)) +

          labs(title = "Distribution of Population ",
               x = NULL,
               y = NULL) +

          theme_classic() +

          theme(plot.title = element_text(size = 14),
                plot.subtitle = element_text(size = 12),
                axis.text = element_text(size = 10),
                axis.title = element_text(size = 10),
                panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = "none")



        p2 <- ggplot() +

          geom_histogram(aes(random_sample[[i]]),
                         col = "black",
                         fill = "grey80") +

          scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                             limits = c(mu - 4*sigma, mu + 4*sigma)) +

          labs(title = paste("Distribution of Sample #", i, sep = ""),
               subtitle = paste("Sample Mean = ", round(sample_means[i], 2), " (from a sample of size n = ", n, ")", sep = ""),
               x = NULL,
               y = NULL) +

          theme_classic() +

          theme(plot.title = element_text(size = 14),
                plot.subtitle = element_text(size = 12),
                axis.text = element_text(size = 10),
                axis.title = element_text(size = 10),
                panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = "none") +

          geom_vline(aes(xintercept = sample_means[i]), lwd = 2, col = "blue")



        p3 <- ggplot() +

          geom_dots(aes(sample_means, fill = after_stat(sample_means == sample_means[i])),
                    layout = "hex",
                    stackratio = 0.9,
                    binwidth = if(zoom == FALSE) {unit(1.5, "mm")} else {unit(3, "mm")},
                    overflow = "compress",
                    col = "grey20",
                    alpha = 0.5) +

          scale_y_continuous(breaks = NULL, labels = NULL) +

          scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                             limits = if(is.na(xlim[1]) == FALSE) {xlim}
                                      else if(zoom == TRUE) {c(min(sample_means) - (diff(range(sample_means))/10),
                                                              max(sample_means) + (diff(range(sample_means))/10))}
                                      else {c(mu - 4*sigma, mu + 4*sigma)}) +

          labs(title = bquote(paste("Sampling Distribution of ", bar(X))),
               subtitle = paste("(simulated using ", reps, " sample means)", sep = ""),
               x = NULL,
               y = NULL) +

          theme_classic() +

          theme(plot.title = element_text(size = 14),
                plot.subtitle = element_text(size = 12),
                axis.text = element_text(size = 10),
                axis.title = element_text(size = 10),
                panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
                axis.line.y = element_blank(),
                legend.position = "none") +

          scale_fill_manual(values = c("grey80", "blue"))



        grid.arrange(p1, p2, p3, ncol = 1, nrow = 3)

      }
    })
  })


}
