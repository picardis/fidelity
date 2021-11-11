#' Function to generate simulated landscapes
#'
#' \code{sim_land} generates a simulated landscape of user-specified ext,
#' resolution, origin, and spatial properties (contagion and constancy)
#'
#' @param res Resolution of the raster in m. Numeric value.
#' @param ext Extent of the raster in m. Numeric value.
#' @param orig Named vector of x and y coordinates of the center of the raster.
#' @param contagion Vector of contagion values to simulate.
#' @param constancy Vector of constancy values to simulate.
#' @return \code{Raster} or \code{RasterStack} of simulated landscapes.
#' @export
sim_land <- function(res,
                     ext,
                     orig,
                     contagion,
                     constancy
) {

  land <- raster::raster(resolution = res,
                 xmn = - ext + orig["x"],
                 ymn= - ext + orig["y"],
                 xmx = ext + orig["x"],
                 ymx = ext + orig["y"])

  g <- as(land, "GridTopology")

  for (i in 1:length(contagion)) {

    model <- RandomFields::RMgauss(var = 1, scale = contagion[i])
    sim2 <- RandomFields::RFsimulate(model, x = g)

    for (j in 1:length(constancy)) {

      sim3 <- as(sim2, "RasterLayer")
      sim3[] <- scales::rescale(raster::values(sim3), to = c(0, 1))^constancy[j]
      sim3[] <- scales::rescale(raster::values(sim3), to = c(-1, 1))

            if(i == 1 & j == 1) {
              sim <- sim3    # if the first one, create new raster
              } else {
                sim <- raster::addLayer(sim, sim3)     # otherwise add a layer
                }

      names(sim)[raster::nlayers(sim)] <- paste0("constancy", round(contagion[i], 1), "_",
                                         "contagion", round(constancy[j], 1))
    }
  }

  return(sim)

}
