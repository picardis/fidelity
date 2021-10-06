#' Function to generate simulated landscapes
#'
#' \code{sim_land} generates a simulated landscape of user-specified ext,
#' resolution, origin, and spatial properties (constancy and clumpiness)
#'
#' @param res Resolution of the raster in m. Numeric value.
#' @param ext Extent of the raster in m. Numeric value.
#' @param orig Named vector of x and y coordinates of the center of the raster.
#' @param constancy Vector of constancy values to simulate.
#' @param clumpiness Vector of clumpiness values to simulate.
#' @return \code{Raster} or \code{RasterStack} of simulated landscapes.
#' @export
sim_land <- function(res,
                     ext,
                     orig,
                     constancy,
                     clumpiness
) {

  land <- raster::raster(resolution = res,
                 xmn = - ext + orig["x"],
                 ymn= - ext + orig["y"],
                 xmx = ext + orig["x"],
                 ymx = ext + orig["y"])

  g <- as(land, "GridTopology")

  for (i in 1:length(constancy)) {

    model <- RandomFields::RMgauss(var = 1, scale = constancy[i])
    sim2 <- RandomFields::RFsimulate(model, x = g)

    for (j in 1:length(clumpiness)) {

      sim3 <- as(sim2, "RasterLayer")
      sim3[] <- scales::rescale(raster::values(sim3), to = c(0, 1))^clumpiness[j]
      sim3[] <- scales::rescale(raster::values(sim3), to = c(-1, 1))

            if(i == 1 & j == 1) {
              sim <- sim3    # if the first one, create new raster
              } else {
                sim <- raster::addLayer(sim, sim3)     # otherwise add a layer
                }

      names(sim)[raster::nlayers(sim)] <- paste0("const", round(constancy[i], 1), "_",
                                         "clump", round(clumpiness[j], 1))
    }
  }

  return(sim)

}
