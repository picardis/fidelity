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
