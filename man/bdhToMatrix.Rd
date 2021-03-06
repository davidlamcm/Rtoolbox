% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_bdhToMatrix.R
\name{bdhToMatrix}
\alias{bdhToMatrix}
\title{return a list of matrix which comprise of a consolidated value matrix and one date matrix given list of bdh data}
\usage{
bdhToMatrix(bdhList, timeSeries, validDays = Inf, lag = 0, cl)
}
\arguments{
\item{bdhList}{list of bdh data to be converted to matrix}

\item{timeSeries}{date or character vector that determine the resulting matrix's rownames}

\item{validDays}{number of days a bdh data is valid since it is published, for price data that only valid for that day, use 1 with lag = 0}

\item{lag}{number of day lag, default is 0}

\item{cl}{cluster that will be used for computation}
}
\description{
return a list of matrix which comprise of a consolidated value matrix and one date matrix given list of bdh data
}
