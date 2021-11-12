#' Get Neighborhoods
#'
#' Function to create a map of neighboring cells in a raster within a
#' user-defined perceptual range distance of each cell.
#'
#' @inheritParams sim_bcrw
#' @return A list with one element per raster cell. Each element contains the
#' identifiers of neighboring cells within the specified perceptual range.
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
