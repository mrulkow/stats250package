
#' Simulation of the Sampling Distribution of the Sample Proportion
#'
#' This function simulates the sampling distribution of the sample proportion with the ability to calculated a simulated p-value.
#'
#' @param p True proportion
#' @param n Sample size
#' @param reps Number of repetitions (default is 100)
#' @param threshold Observed sample proportion used for calculating a simulated p-value
#' @param alternative A character string specifying the direction of the alternative hypothesis ("less", "greater", or "two.sided" (default))
#' @param results Prints out the simulated p-value results when set to TRUE
#' @param zoom Logical argument (TRUE or FALSE) for determining the plot window
#' @param xlim Manually set the x-axis bounds using a vector (default is NA)
#' @param plotresults Displays the simulated p-value results below the plot when set to TRUE
#' @return A simulated sampling distribution of the sample proportion
#' @export
#' @examples
#' simProportions(p = 0.5, n = 20)
#' simProportions(p = 0.5, n = 50, threshold = 0.52, alternative = "greater", results = TRUE)

simProportions <- function(p, n, reps = 100, threshold = NA, alternative = "two.sided", results = FALSE, zoom = TRUE, xlim = NA, plotresults = FALSE){

  # Simulates reps number of sample proportions from samples of size n
  sample_proportions <- rep(0, reps)

  for(i in 1:reps) {
    randomSample <- runif(n)
    sample_proportions[i] <- mean(randomSample < p)
  }

  # Calculate the lower and upper bounds (used for simulated p-value)
  lb <- p - (abs(p - threshold) - .Machine$double.eps ^ 0.5)
  ub <- p + (abs(p - threshold) - .Machine$double.eps ^ 0.5)


  # Create plot
  x <- ggplot() +

    geom_dots(aes(sample_proportions,
                  fill = if(alternative == "greater") {after_stat(sample_proportions >= threshold)}
                  else if(alternative == "less") {after_stat(sample_proportions <= threshold)}
                  else {after_stat(sample_proportions <= lb | sample_proportions >= ub)}),
              scale = if(reps < 25) {0.5} else if (reps < 50) {0.75} else {1},
              binwidth = NA,
              overflow = "compress",
              col = "grey20",
              alpha = 0.5) +

    scale_y_continuous(breaks = NULL, labels = NULL) +

    scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                       limits = if(is.na(xlim[1]) == FALSE) {xlim}
                         else if(zoom == TRUE) {c(max(min(sample_proportions) - (diff(range(sample_proportions))/10), 0),
                                                  min(max(sample_proportions) + (diff(range(sample_proportions))/10), 1))}
                         else {c(max(p - 4*sqrt((p*(1-p))/20), 0), min(p + 4*sqrt((p*(1-p))/20), 1))}) +

    labs(title = bquote(paste("Simulated Sampling Distribution of ", hat(p))),
         subtitle = paste("(from ", reps, " random samples of size n = ", n, ")", sep = ""),
         x = if(!is.na(threshold) & plotresults == TRUE) {
                 if(alternative == "greater") {paste("Simulated p-value: ", sum(sample_proportions >= threshold)/reps, " (", sum(sample_proportions >= threshold), " out of " , reps, ")", sep = "")}
                 else if(alternative == "less") {paste("Simulated p-value: ", sum(sample_proportions <= threshold)/reps, " (", sum(sample_proportions <= threshold), " out of " , reps, ")", sep = "")}
                 else {paste("Simulated p-value: ", sum(sample_proportions <= lb | sample_proportions >= ub)/reps, " (", sum(sample_proportions <= lb | sample_proportions >= ub), " out of " , reps, ")", sep = "")}
         }
         else {NULL},
         y = NULL) +

    theme_classic() +

    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 16),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 12),
          panel.grid.major.x = element_line(color = "grey80",
                                            linetype = "dashed"),
          axis.line.y = element_blank(),
          legend.position = "none") +

    if(!is.na(threshold)) {
      if(alternative == "two.sided") {geom_vline(xintercept = c(lb, ub), linewidth = 1, alpha = 0.5, col = "blue")}
      else {geom_vline(xintercept = threshold, linewidth = 1, alpha = 0.5, col = "blue")}
    }

  finalplot <- x + scale_fill_manual(values = c("grey80", "blue"))
  print(finalplot)

  # Returns results via text
  if(results == TRUE) {
    if(alternative == "greater") {return(paste("Simulated p-value: ", sum(sample_proportions >= threshold)/reps, " (", sum(sample_proportions >= threshold), " out of " , reps, ")", sep = ""))}
    else if(alternative == "less") {return(paste("Simulated p-value: ", sum(sample_proportions <= threshold)/reps, " (", sum(sample_proportions <= threshold), " out of " , reps, ")", sep = ""))}
    else {return(paste("Simulated p-value: ", sum(sample_proportions <= lb | sample_proportions >= ub)/reps, " (", sum(sample_proportions <= lb | sample_proportions >= ub), " out of " , reps, ")", sep = ""))}
  }

}
