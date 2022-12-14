% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_ipsum_dot.R
\name{theme_ipsum_dot}
\alias{theme_ipsum_dot}
\title{A \code{ggplot2} Theme}
\usage{
theme_ipsum_dot()
}
\value{
A modified \code{theme_ipsum()} from hrbrthemes
}
\description{
Returns a modified \code{theme_ipsum()} with a dotted grid and smaller line size.
}
\examples{
library(ggplot2)

ggplot(iris, aes(x = Sepal.Width)) +
  geom_bar() +
  theme_ipsum_dot()
}
