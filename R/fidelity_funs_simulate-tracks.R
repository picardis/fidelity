#' Simulate movement tracks
#'
#' \code{simulate_tracks} is a wrapper function to simulate multiple tracks for
#' multiple scenarios (which can include multiple movement models).
#'
#' @param scenarios Data frame. Output of the create_scenarios() functions (one or multiple rbind together)
#' @inheritParams sim_bcrw
#' @param n_tracks Numeric. Number of tracks to simulate.
#' @return Returns a data frame containing the coordinates of each location along the
#' simulated track of multiple scenarios (including multiple movement models) and the associated simulation parameter values.
#' @export
simulate_tracks <- function(
  scenarios,
  n_steps,
  n_tracks,
  start_loc,
  sl_par,
  prange = NA,
  neighbors = NA,
  cl = NULL
){

  # Initialize output ####

  out <- data.frame()

  # Without parallelization ####

  if (is.null(cl)) {

  # (C)RW ####

  scen_crw <- scenarios[substr(scenarios$scenario_id, 1, 3) == "CRW", ]

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
                  start_loc = start_loc,
                  lands_name = x["lands"])}))

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
      message("Perceptual range unspecified,
      using 95th percentile of step length distribution")
    }

    tmp <- data.frame()

    for (l in 1:length(unique(scen_bcrw$lands))) {

      scen_l <- scen_bcrw[scen_bcrw$lands == unique(scen_bcrw$lands)[l], ]

    # Load raster
    rast <- raster::readAll(raster::raster(as.character(unique(scen_bcrw$lands)[l])))

    for (i in 1:n_tracks) {

      sims_id <- do.call("rbind", apply(scen_l, 1, function(x) {
        sim_bcrw(n_steps = n_steps,
                sl_par = sl_par,
                rho = as.numeric(x["rho"]),
                beta = as.numeric(x["beta"]),
                scenario_id = x["scenario_id"],
                lands = rast,
                start_loc = start_loc,
                prange = prange,
                neighbors = neighbors,
                lands_name = x["lands"])}))

      sims_id <- cbind.data.frame(id = i, sims_id)

      tmp <- rbind(tmp, sims_id)

    }

    }

    out <- rbind(out, tmp)

  }

  }

  # With parallelization ####

  if (!is.null(cl)) {

    # Export parameters to cluster ####

    parallel::clusterExport(cl,
                  varlist = c("scenarios", "n_steps", "n_tracks",
                            "start_loc", "sl_par", "prange", "neighbors"),
                  envir = environment())

    # (C)RW ####

    scen_crw <- scenarios[substr(scenarios$scenario_id, 1, 3) == "CRW", ]

    if (nrow(scen_crw) > 0) {

      res <- do.call("rbind", parallel::parLapply(cl = cl,
                          X = 1:n_tracks,
                          fun = function(track) {
                            sims_id <- do.call("rbind",
                                               apply(scen_crw, 1, function(x) {
                              sim_crw(n_steps = n_steps,
                                      sl_par = sl_par,
                                      rho = as.numeric(x["rho"]),
                                      scenario_id = x["scenario_id"])}))

                            sims_id <- cbind.data.frame(id = track, sims_id)

                          }))

      out <- rbind(out, res)

    }

    # CCRW ####

    scen_ccrw <- scenarios[substr(scenarios$scenario_id, 1, 4) == "CCRW", ]

    if (nrow(scen_ccrw) > 0) {

      res <- do.call("rbind", parallel::parLapply(cl = cl,
                          X = 1:n_tracks,
                          fun = function(track) {

        sims_id <- do.call("rbind", apply(scen_ccrw, 1, function(x) {
          sim_ccrw(n_steps = n_steps,
                   sl_par = sl_par,
                   rho = as.numeric(x["rho"]),
                   boundary_size = as.numeric(x["boundary_size"]),
                   scenario_id = x["scenario_id"])}))

        sims_id <- cbind.data.frame(id = track, sims_id)

                          }))

      out <- rbind(out, res)

    }

    # MCRW ####

    scen_mcrw <- scenarios[substr(scenarios$scenario_id, 1, 4) == "MCRW", ]

    if (nrow(scen_mcrw) > 0) {

      tmp <- data.frame()

      for (l in 1:length(unique(scen_mcrw$lands))) {

        scen_l <- scen_mcrw[scen_mcrw$lands == unique(scen_mcrw$lands)[l], ]

        # Load raster
        rast <- raster::readAll(raster::raster(as.character(unique(scen_mcrw$lands)[l])))

        res <- do.call("rbind", parallel::parLapply(cl = cl,
                            X = 1:n_tracks,
                            fun = function(track) {

          sims_id <- do.call("rbind", apply(scen_l, 1, function(x) {
            sim_mcrw(n_steps = n_steps,
                     sl_par = sl_par,
                     rho = as.numeric(x["rho"]),
                     habitat_effect = as.numeric(x["habitat_effect"]),
                     scenario_id = x["scenario_id"],
                     lands = rast,
                     start_loc = start_loc,
                     lands_name = x["lands"])}))

          sims_id <- cbind.data.frame(id = track, sims_id)

          tmp <- rbind(tmp, sims_id)

        }))

      }

      out <- rbind(out, res)

    }

    # BCRW ####

    scen_bcrw <- scenarios[substr(scenarios$scenario_id, 1, 4) == "BCRW", ]

    if (nrow(scen_bcrw) > 0) {

      if (is.na(prange)) {
        prange <- quantile(rweibull(100000, sl_par[1], sl_par[2]), 0.95)
        message("Perceptual range unspecified,
      using 95th percentile of step length distribution")
      }

      tmp <- data.frame()

      for (l in 1:length(unique(scen_bcrw$lands))) {

        scen_l <- scen_bcrw[scen_bcrw$lands == unique(scen_bcrw$lands)[l], ]

        # Load raster
        rast <- raster::readAll(raster::raster(as.character(unique(scen_bcrw$lands)[l])))

        res <- do.call("rbind", parallel::parLapply(cl = cl,
                            X = 1:n_tracks,
                            fun = function(track) {

          sims_id <- do.call("rbind", apply(scen_l, 1, function(x) {
            sim_bcrw(n_steps = n_steps,
                     sl_par = sl_par,
                     rho = as.numeric(x["rho"]),
                     beta = as.numeric(x["beta"]),
                     scenario_id = x["scenario_id"],
                     lands = rast,
                     start_loc = start_loc,
                     prange = prange,
                     neighbors = neighbors,
                     lands_name = x["lands"])}))

          sims_id <- cbind.data.frame(id = track, sims_id)

          tmp <- rbind(tmp, sims_id)

        }))

      }

      out <- rbind(out, res)

    }

  }

  rownames(out) <- NULL

  # Return result
  return(out)

}
