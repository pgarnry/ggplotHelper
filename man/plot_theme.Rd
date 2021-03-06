% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-theme.R
\name{plot_theme}
\alias{plot_theme}
\title{Function for designing a beautiful theme for ggplot2}
\usage{
plot_theme(legend.position = "bottom", base.size = 10,
  plot.margin = c(0.7, 1.2, 0.5, 0.5), color.theme = "white",
  aspect.ratio = 1.61)
}
\arguments{
\item{legend.position}{character string specifying the legend position (see details)}

\item{base.size}{integer. Sets base font size (see details)}

\item{plot.margin}{numeric vector. Length should be four (see details)}

\item{color.theme}{character string specifying the (pre-defined) color theme - either "white" or "grey"}
}
\description{
Function for designing a beautiful theme for ggplot2
}
\details{
The ggplot2 package comes with four options for setting the
legend position - "left", "right", "bottom" and "top".
}

