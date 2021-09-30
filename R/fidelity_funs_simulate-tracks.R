#' Simulate movement tracks
#'
#' \code{simulate_tracks} is a wrapper function to simulate tracks for multiple
#' scenarios from one or multiple movement models.
simulate_tracks <- function(
  scenarios, # Output of the create_scenarios() functions (one or multiple rbind together)
  n_steps,     # Number of steps to simulate
  n_tracks,        # Number of tracks to simulate
  start_loc = data.frame(x = 0, y = 0), # Center of the raster (0, 0 if unprojected)
  sl_par,   # Shape and scale of Weibull distribution for step length
  prange = NA, # Perceptual range radius (m) for BCRW
  neighbors = NA # Path to file of cell neighborhoods for BCRW
){

  # (C)RW ####

  scen_crw <- scenarios[substr(scenarios$scenario_id, 1, 3) == "CRW", ]

  out <- data.frame()

  if (nrow(scen_crw) > 0) {

    for (i in 1:n_tracks) {

      sims_id <- do.call("rbind", apply(scen_crw, 1, function(x) {
        sim_crw(n_steps = n_steps,
                sl_par = sl_par,
                rho = as.numeric(x["rho"]),
                scenario_id = x["scenario_id"])}))

      sims_id <- cbind.data.frame(id = i, sims_id)

      out <- rbind(out, sims_id)

    }

  }

  # CCRW ####

  scen_ccrw <- scenarios[substr(scenarios$scenario_id, 1, 4) == "CCRW", ]

  if (nrow(scen_ccrw) > 0) {

    for (i in 1:n_tracks) {

      sims_id <- do.call("rbind", apply(scen_ccrw, 1, function(x) {
        sim_ccrw(n_steps = n_steps,
                sl_par = sl_par,
                rho = as.numeric(x["rho"]),
                boundary_size = as.numeric(x["boundary_size"]),
                scenario_id = x["scenario_id"])}))

      sims_id <- cbind.data.frame(id = i, sims_id)

      out <- rbind(out, sims_id)

    }

  }

  # MCRW ####

  scen_mcrw <- scenarios[substr(scenarios$scenario_id, 1, 4) == "MCRW", ]

  if (nrow(scen_mcrw) > 0) {

    tmp <- data.frame()

    for (l in 1:length(unique(scen_mcrw$lands))) {

      scen_l <- scen_mcrw[scen_mcrw$lands == unique(scen_mcrw$lands)[l], ]

      # Load raster
      rast <- raster::readAll(raster::raster(as.character(unique(scen_mcrw$lands)[l])))

      for (i in 1:n_tracks) {

        sims_id <- do.call("rbind", apply(scen_l, 1, function(x) {
          sim_mcrw(n_steps = n_steps,
                  sl_par = sl_par,
                  rho = as.numeric(x["rho"]),
                  habitat_effect = as.numeric(x["habitat_effect"]),
                  scenario_id = x["scenario_id"],
                  lands = rast,
                  start_loc = start_loc)}))

        sims_id <- cbind.data.frame(id = i, sims_id)

        tmp <- rbind(tmp, sims_id)

      }

    }

    out <- rbind(out, tmp)

  }

  # BCRW ####

  scen_bcrw <- scenarios[substr(scenarios$scenario_id, 1, 4) == "BCRW", ]

  if (nrow(scen_bcrw) > 0) {

    if (is.na(prange)) {
      prange <- quantile(rweibull(100000, sl_par[1], sl_par[2]), 0.95)
      warning("Perceptual range unspecified,
      using 95th percentile of step length distribution")
    }

    if (is.na(neighbors)) {
      stop("Path to cell neighborhoods file is missing")
    }

    tmp <- data.frame()

    for (l in 1:length(unique(scen_bcrw$lands))) {

      scen_l <- scen_bcrw[scen_bcrw$lands == unique(scen_bcrw$lands)[l], ]

    # Load raster
    rast <- raster::readAll(raster::raster(as.character(unique(scen_bcrw$lands)[l])))

    # Load cell neighborhoods
    neighbors <- tryCatch(readRDS(as.character(neighbors)),
                          error = function(e) {
                            paste0("Could not load ", neighbors_path)
                            })

    for (i in 1:n_tracks) {

      sims_id <- do.call("rbind", apply(scen_l, 1, function(x) {
        sim_bcrw(n_steps = n_steps,
                sl_par = sl_par,
                rho = as.numeric(x["rho"]),
                beta = as.numeric(x["beta"]),
                scenario_id = x["scenario_id"],
                lands = rast,
                start_loc = start_loc)}))

      sims_id <- cbind.data.frame(id = i, sims_id)

      tmp <- rbind(tmp, sims_id)

    }

    }

    out <- rbind(out, tmp)

  }

  rownames(out) <- NULL

  # Return result
  return(out)

}
