% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fidelity_funs_calculate-return-intensity.R
\name{calc_spint}
\alias{calc_spint}
\title{Calculate Spatial Intensity of Returns (Multiple Tracks)}
\usage{
calc_spint(rets, dist, lag)
}
\arguments{
\item{rets}{Data frame of returns obtained from \code{calc_returns()}.}

\item{dist}{Numeric. Distance used to calculate returns. Unit should
match the unit of the coordinates: m if UTM, degrees if lat/long.}

\item{lag}{Numeric. Temporal lag of returns (unit is locations, e.g., 42 for
one week at 6h resolution). Must match one of the lags used in
\code{calc_returns()}.}
}
\value{
A data frame with 2 columns, one for scenario ID and one for the
number of returns to each revisited location. For each scenario, the data
frame will include as many rows as the number of locations that were
revisited at least once.
}
\description{
Function to calculate spatial intensity of returns.
}
