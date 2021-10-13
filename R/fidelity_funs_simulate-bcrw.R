#' Simulate Biased Correlated Random Walk (BCRW)
#'
#' \code{sim_bcrw} simulates a movement track with a Biased Correlated Random Walk model.
#'
#' @param n_steps Numeric. Number of steps to simulate
#' @param sl_par Vector of length 2 containing the shape and scale of the step
#' length distribution (Weibull)
#' @param rho Numeric. Movement autocorrelation parameter
#' @param start_loc Data frame with coordinates of the starting location
#' (columns named x and y). 0, 0 if unprojected.
#' @param beta Numeric. Bias applied to the Biased Correlated Random Walk
#' @param prange Numeric. Perceptual range radius (m) for BCRW.
#' @param lands \code{Raster} of habitat quality (landscape)
#' @param neighbors \code{Raster} of the distance to neighboring cells (neighborhoods)
#' @param scenario_id Numeric. Optional ID of the current scenario. Automatically assigned
#' when using simulate_tracks().
#' @param lands_name Path to the landscape file
#' @return Returns a data frame containing the coordinates of each location along the
#' simulated track and the associated simulation parameter values.
#' @export
sim_bcrw <- function(n_steps,
                     sl_par,
                     rho,
                     start_loc,
                     beta,
                     prange,
                     lands,
                     neighbors,
                     scenario_id = NA,
                     lands_name = NA
                     ){

  if (is.na(prange)) {
    # Default perceptual range is 95% quantile of step length distribution
    prange <- quantile(rweibull(10000, sl_par[1], sl_par[2]), 0.95)
  }

  # Jitter starting points
  x <- c()
  x[1] <- runif(n = 1,
                min = start_loc$x - 20000,
                max = start_loc$x + 20000)
  y <- c()
  y[1] <- runif(n = 1,
                min = start_loc$y - 20000,
                max = start_loc$y + 20000)

  # Initialize output: data frame of coordinates, start with first location
  out <- data.frame(x = x, y = y)

  # Need to initialize first step before starting the loop.

  # Initialize vector of bearings
  bbias <- c()

  # To calculate first bearing:
  # 1. Get cell ID of starting point.
  start_cell <- raster::cellFromXY(lands,
                           xy = c(out[, 1],
                                  out[, 2]))

  # 2. Get coordinates of neighbors of current position.
  mat <- raster::xyFromCell(lands, neighbors[[start_cell]])
  # Store the position of the current cell in the vector of neighbors
  pos <- which(neighbors[[start_cell]] == start_cell)

  # 3. Calculate distance of each cell in the neighborhood to the current position
  # Select distances to that among all pairwise distances
  dists <- as.matrix(dist(mat))[pos, ]
  # Filter points within the perceptual range
  within_prange <- mat[which(dists < prange), ]
  # Get the cell numbers for these points
  cells <- neighbors[[start_cell]][which(dists < prange)]
  # Extract habitat values
  val <- raster::extract(lands, cells)
  # Get distances
  dist <- dists[which(dists < prange)]
  # Combine
  cell_info <- cbind.data.frame(within_prange, cells, val, dist)
  # Exclude the current position or the animal won't move
  cell_info <- cell_info[cell_info$cell != start_cell, ]
  # Rank cells based on best habitat value and lower distance and select the top
  end_point <- cell_info[order(-cell_info$val, cell_info$dist), ][1, ]

  # 4. Calculate the bearing from the current position to the attraction cell
  dx <- end_point[, 1] - out[[1]]
  dy <- end_point[, 2] - out[[2]]
  bbias[1] <- atan2(y = dy, x = dx)

  # 5. Randomize the starting direction
  start_angle <- as.numeric(circular::rwrappedcauchy(n = 1))

  # 6. Initialize other vectors
  # Delta y and x components of the angle for a unit turn
    ya <- c() # This is the expected delta y if the step had length 1
    ya[1] <- (1 - beta) * sin(start_angle) + beta * sin(bbias[1])
    xa <- c() # This is the expected delta x if the step had length 1
    xa[1] <- (1 - beta) * cos(start_angle) + beta * cos(bbias[1])

  # 7. Derive expected angle from delta x and y
  # (based on bias alone, no directional persistence)
  ta <- c()
  ta[1] <- atan2(y = ya[1], x = xa[1])

  # 8. Initialize vector of simulated angles accounting for directional persistence and bias
  angles <- c()
  # For step 1, we have no directional persistence because there's no previous step
  # So angle[1] and ta[1] are the same
  angles[1] <- ta[1]

  # 9. Draw value of first step
    steps <- c()
    steps[1] <- rweibull(n = 1, shape = sl_par[1], scale = sl_par[2])

  # 10. Calculate coordinates of end point
  xy_end <- end_coords(steps[1], angles[1], x[1], y[1])
  x[2] <- xy_end[, 1]
  y[2] <- xy_end[, 2]

  # Loop over steps from the second step onward
  for (t in 2:n_steps) {

    # 1. Identify patch of attraction
    cell <- raster::cellFromXY(lands, c(x[t], y[t]))
    mat <- raster::xyFromCell(lands, neighbors[[cell]])
    pos <- which(neighbors[[cell]] == cell)
    dists <- as.matrix(dist(mat))[pos, ]
    within_prange <- mat[which(dists < prange), ]
    cells <- neighbors[[cell]][which(dists < prange)]
    val <- raster::extract(lands, cells)
    dist <- dists[which(dists < prange)]
    cell_info <- cbind.data.frame(within_prange, cells, val, dist)
    cell_info <- cell_info[cell_info$cell != start_cell, ]
    end_point <- cell_info[order(-cell_info$val, cell_info$dist), ][1, ]
    dx <- end_point[, 1] - x[t]
    dy <- end_point[, 2] - y[t]
    bbias[t] <- atan2(y = dy, x = dx)
    ya[t] <- (1 - beta) * sin(ta[t - 1]) + beta * sin(bbias[t])
    xa[t] <- (1 - beta) * cos(ta[t - 1]) + beta * cos(bbias[t])
    ta[t] <- atan2(y = ya[t], x = xa[t])
    angles[t] <- CircStats::rwrpcauchy(n = 1, location = ta[t], rho = rho)
    steps[t] <- rweibull(n = 1, shape = sl_par[1], scale = sl_par[2])

    # 5. Get coordinates of end point
    xy_end <- end_coords(steps[t], angles[t], x[t], y[t])
    x[t+1] <- xy_end[, 1]
    y[t+1] <- xy_end[, 2]

    # Censor animal if it walks off of the landscape
    if (is.na(raster::cellFromXY(lands, c(x[t + 1], y[t + 1])))) { break }

  }

  out <- data.frame(x = x, y = y)

  out$step <- 0:n_steps
  out$rho <- rho
  out$boundary_size <- NA
  out$habitat_effect <- NA
  out$beta <- beta
  out$landscape <- lands_name
  out$scenario_id <- scenario_id

  return(out)

}
