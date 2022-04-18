#' Create Correlated Random Walk Scenarios (CRW)
#'
#' Function to generate one or more Correlated Random Walk scenario/s.
#'
#' @param rho Numeric vector (of any length) of autocorrelation parameter values
#' @return Returns a data frame describing scenario/s, one row per scenario
#' for a total of \code{length(rho)} rows.
#' Includes columns \code{rho},
#' \code{boundary_size} (set to \code{NA} because not applicable to CRW),
#' \code{habitat_effect} (set to \code{NA} because not applicable to CRW),
#' \code{beta} (set to \code{NA} because not applicable to CRW), \code{lands}
#' (set to \code{NA} because not applicable to CRW), and \code{scenario_id}
#' (unique identifier for the scenario).
#' @export
create_scenarios_crw <- function(
  rho
  ) {

  if (any(is.na(rho))) {
    stop("CRW parameters cannot contain missing values")
  }

  cat(paste0("Creating ",
             length(rho),
             " CRW scenarios \n"))

  scen <- expand.grid(rho = rho,
                      boundary_size = NA,
                      habitat_effect = NA,
                      beta = NA,
                      lands = NA,
                      kappa = NA,
                      chi = NA)

  scen$scenario_id <- paste0("CRW_", 1:nrow(scen))

  return(scen)

}

#' Create Constrained Correlated Random Walk Scenarios (CCRW)
#'
#' Function to generate one or more Constrained Correlated Random Walk scenario/s.
#'
#' @param rho Numeric vector (of any length) of autocorrelation parameter values
#' @param boundary_size Numeric vector (of any length) specifying values for the
#' size of the bounding box (length of the side). Units should be m if the
#' user intends to use UTM coordinates for the simulation or degrees for lat/long.
#' @return Returns a data frame describing scenario/s, one row per scenario
#' using all possible combinations of the specified parameter values, for a
#' total of \code{length(rho) * length(boundary_size)} rows.
#' Includes columns \code{rho},
#' \code{boundary_size},
#' \code{habitat_effect} (set to \code{NA} because not applicable to CCRW),
#' \code{beta} (set to \code{NA} because not applicable to CCRW), \code{lands}
#' (set to \code{NA} because not applicable to CCRW), and \code{scenario_id}
#' (unique identifier for the scenario).
#' @export

create_scenarios_ccrw <- function(
  rho,
  boundary_size
) {

  if (any(is.na(rho)) |
      any(is.na(boundary_size))) {
    stop("CCRW parameters cannot contain missing values")
  }

  cat(paste0("Creating ", length(rho) *
               length(boundary_size),
             " CCRW scenarios \n"))

  scen <- expand.grid(rho = rho,
                      boundary_size = boundary_size,
                      habitat_effect = NA,
                      beta = NA,
                      lands = NA,
                      kappa = NA,
                      chi = NA)

  scen$scenario_id <- paste0("CCRW_", 1:nrow(scen))

  return(scen)

}

#' Create Mixture Correlated Random Walk Scenarios (MCRW)
#'
#' Function to generate one or more Mixture Correlated Random Walk scenario/s.
#'
#' @param rho Numeric vector (of any length) of autocorrelation parameter values
#' @param habitat_effect Numeric vector (of any length) specifying values for
#' the strength of the habitat effect.
#' @param lands Character vector (of any length) providing the paths to the
#' landscape rasters to be loaded from disk and used in the simulation.
#' @return Returns a data frame describing scenario/s, one row per scenario
#' using all possible combinations of the specified parameter values, for a
#' total of \code{length(rho) * length(habitat_effect) * length(lands)} rows.
#' Includes columns \code{rho},
#' \code{boundary_size} (set to \code{NA} because not applicable to MCRW),
#' \code{habitat_effect},
#' \code{beta} (set to \code{NA} because not applicable to MCRW), \code{lands},
#' and \code{scenario_id} (unique identifier for the scenario).
#' @export

create_scenarios_mcrw <- function(
  rho,
  habitat_effect,
  lands
) {

  if (any(is.na(rho)) |
      any(is.na(habitat_effect))) {
    stop("MCRW parameters cannot contain missing values")
  }

  if (any(is.na(lands))) {
    stop("Landscape cannot be missing")
  }

  cat(paste0("Creating ", length(rho) *
               length(habitat_effect) *
               length(lands),
             " MCRW scenarios \n"))

  scen <- expand.grid(rho = rho,
                      boundary_size = NA,
                      habitat_effect = habitat_effect,
                      beta = NA,
                      lands = lands,
                      kappa = as.numeric(str_extract(lands,
                                          "(?<=kappa_)(.+)(?=_chi)")),
                      chi = as.numeric(str_extract(lands,
                                        "(?<=chi_)(.+)(?=.tif)")))

  scen$scenario_id <- paste0("MCRW_", 1:nrow(scen))

  return(scen)

}

#'  Create Biased Correlated Random Walk Scenarios (BCRW)
#'
#' Function to generate one or more Biased Correlated Random Walk scenario/s.
#'
#' @param rho Numeric vector (of any length) of autocorrelation parameter values
#' @param beta Numeric vector (of any length) specifying values for
#' the bias parameter.
#' @param lands Character vector (of any length) providing the paths to the
#' landscape rasters to be loaded from disk and used in the simulation.
#' @return Returns a data frame describing scenario/s, one row per scenario
#' using all possible combinations of the specified parameter values, for a
#' total of \code{length(rho) * length(beta) * length(lands)} rows.
#' Includes columns \code{rho},
#' \code{boundary_size} (set to \code{NA} because not applicable to BCRW),
#' \code{habitat_effect} (set to \code{NA} because not applicable to BCRW),
#' \code{beta}, \code{lands},
#' and \code{scenario_id} (unique identifier for the scenario).
#' @export

create_scenarios_bcrw <- function(
  rho,
  beta,
  lands
) {

  if (any(is.na(rho)) |
      any(is.na(beta))) {
    stop("BCRW parameters cannot contain missing values")
  }

  if (any(is.na(lands))) {
    stop("Landscape cannot be missing")
  }

  cat(paste0("Creating ", length(rho) *
               length(beta) *
               length(lands),
             " BCRW scenarios \n"))

  scen <- expand.grid(rho = rho,
                      boundary_size = NA,
                      habitat_effect = NA,
                      beta = beta,
                      lands = lands,
                      kappa = as.numeric(str_extract(lands,
                                                     "(?<=kappa_)(.+)(?=_chi)")),
                      chi = as.numeric(str_extract(lands,
                                                   "(?<=chi_)(.+)(?=.tif)")))

  scen$scenario_id <- paste0("BCRW_", 1:nrow(scen))

  return(scen)

}
