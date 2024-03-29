% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demoMeans.R
\name{demoMeans}
\alias{demoMeans}
\title{Instructor Demo Tool for the Sampling Distribution of the Sample Mean}
\usage{
demoMeans(mu, sigma, n, reps = 100, show = 5, zoom = TRUE, xlim = NA)
}
\arguments{
\item{mu}{Instead of providing data, the population mean (mu) and population standard deviation (sigma) can be specified for a normal distribution}

\item{sigma}{Instead of providing data, the population mean (mu) and population standard deviation (sigma) can be specified for a normal distribution}

\item{n}{Sample size}

\item{reps}{Number of repetitions (default is 500)}

\item{show}{Number of sample mean examples to walk through (default is 5)}

\item{zoom}{Logical argument (TRUE or FALSE) for determining the plot window}

\item{xlim}{Manually set the x-axis bounds using a vector (default is NA)}
}
\value{
Three plots: the distribution of the population, the distribution of the sample, and a simulated sampling distribution of the sample mean
}
\description{
This function simulates the sampling distribution of the sample mean
}
\examples{
demoMeans(mu = 100, sigma = 10, n = 50)
demoMeans(mu = 100, sigma = 10, n = 20, show = 2, xlim = c(90, 110))
}
