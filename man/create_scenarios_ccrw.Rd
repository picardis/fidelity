% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fidelity_funs_create-scenarios.R
\name{create_scenarios_ccrw}
\alias{create_scenarios_ccrw}
\title{Create Constrained Correlated Random Walk Scenarios (CCRW)}
\usage{
create_scenarios_ccrw(rho, boundary_size)
}
\arguments{
\item{rho}{Numeric vector (of any length) of autocorrelation parameter values}

\item{boundary_size}{Numeric vector (of any length) specifying values for the
size of the bounding box (length of the side). Units should be m if the
user intends to use UTM coordinates for the simulation or degrees for lat/long.}
}
\value{
Returns a data frame describing scenario/s, one row per scenario
using all possible combinations of the specified parameter values, for a
total of \code{length(rho) * length(boundary_size)} rows.
Includes columns \code{rho},
\code{boundary_size},
\code{habitat_effect} (set to \code{NA} because not applicable to CCRW),
\code{beta} (set to \code{NA} because not applicable to CCRW), \code{lands}
(set to \code{NA} because not applicable to CCRW), and \code{scenario_id}
(unique identifier for the scenario).
}
\description{
Function to generate one or more Constrained Correlated Random Walk scenario/s.
}
