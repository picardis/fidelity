#' Simulate movement tracks
#'
#' \code{simulate_tracks} is a wrapper function to simulate tracks from multiple
#' movement models.
#' @inheritParams sim_crw
simulate_tracks <- function(
  start_loc = data.frame(x = 0, y = 0), # Center of the raster (0, 0 if unprojected)
  sl_par = c(NA, NA),   # Shape and scale of Weibull distribution for step length
  n_steps = NA,     # Number of steps to simulate
  n_tracks = NA,        # Number of tracks to simulate
  rho = NA,   # Value of rho for Wrapped Cauchy distribution of turning angles
  boundary_size = NA,   # Size of boundary for CCRW
  lands = NA,   # Raster of habitat quality (file path)
  habitat_effect = NA,   # Strength of habitat effect for MCRW
  beta = NA,     # Bias for BCRW
  prange = NA, # Perceptual range radius (m)
  neighbors = NA, # Path to file of cell neighborhoods
  model = "all" # Which models should be run? A vector with one of more of these
  # possible entries: "crw", "ccrw", "mcrw", "bcrw", "all". Defaults to "all"
){

  # Load packages
  require(circular)
  require(CircStats)
  require(stringr)
  require(raster)
  require(dplyr)

  #---------------#
  # (C)RW ####
  #---------------#
  if("crw" %in% model | model == "all") {

    # lapply to each track ID
    tmp <- do.call(rbind, lapply(1:n_tracks, sim_crw))

  }

  #------------------------#
  # CCRW ####
  #------------------------#

  if("ccrw" %in% model | model == "all") {

    # lapply to each track ID
    tmp <- do.call(rbind, lapply(1:ids, sim_ccrw))

  }

  #-------------------#
  # MCRW ####
  #-------------------#
  if("mcrw" %in% model | model == "all") {

    # Load raster
    lands <- tryCatch(readAll(raster(as.character(lands))),
                      error = function(e) {
                        paste0("Could not load raster", lands)
                        })

    # lapply to each track ID
    tmp <- do.call(rbind, lapply(1:ids, sim_mcrw))

  }

  #--------------------#
  # BCRW ####
  #--------------------#
  if("bcrw" %in% model | model == "all") {

    # Load raster
    lands <- tryCatch(readAll(raster(as.character(lands))),
                      error = function(e) {
                        paste0("Could not load raster ", land)
                        })

    # Load cell neighborhoods
    neighbors <- tryCatch(readRDS(as.character(neighbors)),
                          error = function(e) {
                            paste0("Could not load ", neighbors_path)
                            })

    # lapply to each track ID
    tmp <- do.call(rbind, lapply(1:ids, sim_bcrw))
  }

  # Return result
  return(tmp)

}
