#' Create Correlated Random Walk Scenario(s) (CRW)
#'
#' \code{create_scenarios_crw} generates Correlated Random Walk scenario(s).
#'
#' @inheritParams sim_crw
#' @return Returns a data frame of all the possible Correlated Random Walk Scenario(s) based on rho values. Na is applied to Boundary_size, habitat_effect, beta, and lands.
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
                      lands = NA)

  scen$scenario_id <- paste0("CRW_", 1:nrow(scen))

  return(scen)

}

#'  Create Constrained Correlated Random Walk Scenario(s) (CCRW)
#'
#' \code{create_scenarios_ccrw} generates Constrained Correlated Random Walk scenario(s).
#'
#' @inheritParams sim_ccrw
#' @return Returns a data frame of all the possible Constrained Correlated Random Walk Scenario(s) based on rho and boundary size values. Na is applied to habitat_effect, beta, and lands.
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
                      lands = NA)

  scen$scenario_id <- paste0("CCRW_", 1:nrow(scen))

  return(scen)

}

#'  Create Mixture Correlated Random Walk Scenario(s) (MCRW)
#'
#' \code{create_scenarios_mcrw} generates Mixture Correlated Random Walk scenario(s).
#'
#' @inheritParams sim_mcrw
#' @return Returns a data frame of all the possible Mixture Correlated Random Walk Scenario(s) based on rho, habitat effect, and lands values. Na is applied to boundary size and beta.
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
                      lands = lands)

  scen$scenario_id <- paste0("MCRW_", 1:nrow(scen))

  return(scen)

}

#'  Create Biased Correlated Random Walk Scenario(s) (BCRW)
#'
#' \code{create_scenarios_bcrw} generates Biased Correlated Random Walk scenario(s).
#'
#' @inheritParams sim_bcrw
#' @return Returns a data frame of all the possible Biased Correlated Random Walk Scenario(s) based on rho, beta, and lands values. Na is applied to boundary size and habitat effect.
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
                      lands = lands)

  scen$scenario_id <- paste0("BCRW_", 1:nrow(scen))

  return(scen)

}
