% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_BRFSS.R
\name{clean_BRFSS}
\alias{clean_BRFSS}
\title{Clean the BRFSS data}
\usage{
clean_BRFSS(df)
}
\arguments{
\item{df}{}
}
\value{
A data frame
}
\description{
Write all cleaning process in this file
}
\examples{
read_BRFSS(2000) \%>\% clean_BRFSS()
lapply(1995:2010, read_BRFSS) \%>\% merge_BRFSS() \%>\% clean_BRFSS()

}
\author{
Jiacheng He
}
