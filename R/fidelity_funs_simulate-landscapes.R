#' Generate simulated landscapes
#'
#' Function to generate one or a set of simulated landscapes of user-specified
#' extent, resolution, origin, and spatial properties (contagion and
#' heterogeneity)
#'
#' @param res Numeric. Resolution of the raster (units match the units of the
#' coordinates, m if UTM and degrees if lat/long)
#' @param ext Numeric. Raster extent (units match the units of the coordinates,
#' m if UTM and degrees if lat/long)
#' @param orig Named vector of x and y coordinates of the center of the raster.
#' @param kappa Numeric vector (any length) of contagion values to simulate.
#' @param chi Numeric vector (any length) of heterogeneity values to simulate.
#' @return \code{Raster} or \code{RasterStack} of simulated landscapes. As many
#' as \code{length(kappa) * length(chi)}.
#' @export
sim_land <- function(res,
                     ext,
                     orig,
                     kappa,
                     chi
) {

  land <- raster::raster(resolution = res,
                 xmn = - ext + orig["x"],
                 ymn= - ext + orig["y"],
                 xmx = ext + orig["x"],
                 ymx = ext + orig["y"])

  g <- as(land, "GridTopology")

  for (i in 1:length(kappa)) {

    model <- RandomFields::RMgauss(var = 1, scale = kappa[i])
    sim2 <- RandomFields::RFsimulate(model, x = g)

    for (j in 1:length(chi)) {

      sim3 <- as(sim2, "RasterLayer")
      sim3[] <- scales::rescale(raster::values(sim3), to = c(0, 1))^chi[j]
      sim3[] <- scales::rescale(raster::values(sim3), to = c(-1, 1))

            if(i == 1 & j == 1) {
              sim <- sim3    # if the first one, create new raster
              } else {
                sim <- raster::addLayer(sim, sim3)     # otherwise add a layer
                }

      names(sim)[raster::nlayers(sim)] <- paste0("kappa_", round(kappa[i], 1), "_",
                                         "chi_", round(chi[j], 1))
    }
  }

  return(sim)

}
