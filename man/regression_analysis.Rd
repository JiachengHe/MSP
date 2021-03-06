% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression_analysis.R
\name{regression_analysis}
\alias{regression_analysis}
\title{Run regression}
\usage{
regression_analysis(df, model, cluster = NULL, event_plot = FALSE,
  xlab = NULL, ylab = NULL)
}
\arguments{
\item{df}{The data frame that contains the regression variables}

\item{model}{The regression model}

\item{cluster}{Whether to cluster the standard error. A character of the cluster variable name}

\item{event_plot}{Whether to generate the event study plot}

\item{xlab}{The xlabel of the event plot}

\item{ylab}{The ylabel of the event plot}
}
\value{
A list of regression result (and an event plot if specified)
}
\description{
Write all regression procedure in this file
}
\examples{

}
\author{
Jiacheng He
}
