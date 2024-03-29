% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleProportion.R
\name{sampleProportion}
\alias{sampleProportion}
\title{Sample Proportion}
\usage{
sampleProportion(data, n, operator = `==`, value = "yes")
}
\arguments{
\item{data}{Variable of interest (dataset$variable)}

\item{n}{Sample size}

\item{operator}{Symbol for the desired logical expression (>=, <, ==, etc.). Must be surrounded by backticks (``)}

\item{value}{Value used in the desired logical expression}
}
\value{
A random sample of size n and its corresponding sample proportion
}
\description{
Collects a random sample of n observations and computes the sample proportion based on a specified logical expression
}
\examples{
sampleProportion(trees$Height, n = 10, operator = `>=`, value = 78)
sampleProportion(iris$Species, n = 30, operator = `==`, value = "setosa")
}
