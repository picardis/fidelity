#' Simulate Correlated Random Walk (CRW)
#'
#' Simulate a movement track with a Correlated Random Walk model.
#'
#' @param n_steps Numeric. Number of steps to simulate
#' @param sl_par Vector of length 2 containing the shape and scale of the step
#' length distribution (Weibull)
#' @param rho Numeric. Movement autocorrelation parameter
#' @param start_loc Data frame with coordinates of the starting location
#' (columns named x and y)
#' @return A data frame containing the coordinates of each location along the
#' simulated track and the associated simulation parameter values.
sim_crw <- function(n_steps,
                    sl_par,
                    rho,
                    start_loc = data.frame(x = 0, y = 0),
                    scenario_id = NA
                    ) {

  # Draw steps
  steps <- rweibull(n_steps + 1, sl_par[1], sl_par[2])
  # Draw angles
  angles <- cumsum(c(runif(1, 0, 2 * pi),
                 as.numeric(circular::rwrappedcauchy(n_steps,
                                           circular::circular(0),
                                           rho,
                                           control.circular=list(units="radians")))))

  # Compute coordinates
  x <- c(start_loc$x[1], start_loc$x[1] + cumsum(cos(angles) * steps))
  y <- c(start_loc$y[1], start_loc$y[1] + cumsum(sin(angles) * steps))

  # Create output data frame
  out <- data.frame(x = x, y = y)

  # Add remaining parameters
  out$step <- 0:(n_steps + 1)
  out$rho <- rho
  out$boundary_size <- NA
  out$habitat_effect <- NA
  out$landscape <- NA
  out$beta <- NA
  if (!is.na(scenario_id)) {
    out$scenario_id <- scenario_id
    }

  return(out)
}
