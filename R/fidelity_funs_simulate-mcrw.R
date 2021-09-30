# Simulate Mixture Correlated Random Walk (MCRW)

sim_mcrw <- function(n_steps, # Number of steps to simulate
                     sl_par, # Parameters of Weibull distribution
                     rho, # Movement autocorrelation parameter
                     start_loc, # Coordinates of the starting location
                     habitat_effect, # Strength of habitat response
                     lands, # Raster of habitat quality
                     scenario_id = NA,
                     lands_name = NA
                     ) {

  # Jitter starting points
  start_loc$x <- runif(n = 1,
                       min = start_loc$x - 20000,
                       max = start_loc$x + 20000)
  start_loc$y <- runif(n = 1,
                       min = start_loc$y - 20000,
                       max = start_loc$y + 20000)

  # Initialize output
  out <- start_loc

  # Habitat quality
  qual <- c()
  qual[1] <- raster::extract(lands, out[1, ])

  # Initialize vector of angles and draw first one
  angles <- c()
  angles[1] <- runif(1, 0, 2 * pi)

  for(s in 2:(n_steps + 2)) {

    sl_multiplier <- 10 * habitat_effect
    new_shape <- qual[s - 1] * sl_multiplier * sl_par[2]
    steps <- rweibull(1,
                      sl_par[1],
                      sl_par[2] - (ifelse(new_shape > sl_par[2],
                                          sl_par[2] * 0.99,
                                          new_shape)))

    rho_new <- ifelse(rho - (qual[s - 1] * habitat_effect * rho) > 1,
                      1,
                      rho - (qual[s - 1] * habitat_effect * rho))
    angles[s] <- sum(c(angles[s - 1],
                       as.numeric(circular::rwrappedcauchy(1,
                                                           circular::circular(0),
                                                           rho_new,
                                                           control.circular=list(
                                                             units="radians")))))

    tmp <- data.frame(x = out$x[s - 1] + cos(angles[s]) * steps,
                      y = out$y[s - 1] + sin(angles[s]) * steps)
    qual[s] <- raster::extract(lands, tmp)

    # Censor animals that walk off of the landscape
    if (is.na(qual[s]) == TRUE) { break }

    out <- rbind(out, tmp)

     }

  out$step <- 0:(n_steps + 1)
  out$rho <- rho
  out$boundary_size <- NA
  out$habitat_effect <- habitat_effect
  out$beta <- NA
  out$beta <- NA
  out$landscape <- lands_name
  out$scenario_id <- scenario_id

  return(out)

}
