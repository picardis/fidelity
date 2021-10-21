#' Get Neighborhoods
#'
#' \code{get_neighbors} creates a vector of neighbors in the perceptual range for each cell in a landscape.
#'
#' @inheritParams sim_bcrw
#' @return Returns a list of the number of neighbors within the perceptual range for each cell in a landscape
#' @export

get_neighbors <- function(lands,
                          prange) {

  # Resolution of the raster
  rsl <- raster::res(lands)

  # Restrict the range of plausible cells based on
  # the ratio of perceptual range/cell size
  n_cells <- ceiling(prange/rsl)

  # Neighborhood matrix
  nm <- matrix(data = 1,
               nrow = (n_cells * 2) + 1,
               ncol = (n_cells * 2) + 1)

  # Assign 0 to the central cell
  nm[n_cells + 1, n_cells + 1] <- 0

  # Get IDs of cells in the neighborhood of each cell
  neigh <- lapply(1:ncell(lands),
                  function(cell) {
                    raster::adjacent(x = sub,
                                     cells = cell,
                                     directions = nm,
                                     pairs = FALSE)
                  })

  return(neigh)

}
