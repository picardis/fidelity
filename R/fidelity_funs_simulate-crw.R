#' Simulate Correlated Random Walk (CRW)
#'
#' Function to simulate a movement track under a Correlated Random Walk model.
#' Called as needed by \code{simulate_tracks}.
#'
#' @inheritParams sim_bcrw
#' @return A data frame containing the coordinates of each location along the
#' simulated track and the associated simulation parameter values as specified
#' in \code{create_scenarios_crw}.
#' @export
sim_crw <- function(n_steps,
                    sl_par,
                    rho,
                    start_loc = data.frame(x = 0, y = 0),
                    scenario_id = NA # Scenario identifier (structure: mod_num)
                    ) {

  if (is.na(rho)) {stop("Value of rho is NA")}

  # Draw steps
  steps <- rweibull(n_steps, sl_par[1], sl_par[2])
  # Draw angles
  angles <- cumsum(c(runif(1, 0, 2 * pi),
                 as.numeric(circular::rwrappedcauchy(n_steps - 1,
                                           circular::circular(0),
                                           rho,
                                           control.circular=list(units="radians")))))

  # Compute coordinates
  x <- c(start_loc$x[1], start_loc$x[1] + cumsum(cos(angles) * steps))
  y <- c(start_loc$y[1], start_loc$y[1] + cumsum(sin(angles) * steps))

  # Create output data frame
  out <- data.frame(x = x, y = y)

  # Add remaining parameters
  out$step <- 0:(nrow(out)-1)
  out$rho <- rho
  out$boundary_size <- NA
  out$habitat_effect <- NA
  out$beta <- NA
  out$landscape <- NA
  out$scenario_id <- scenario_id

  return(out)
}
