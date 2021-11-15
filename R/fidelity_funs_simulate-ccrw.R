#' Simulate Constrained Correlated Random Walk (CCRW)
#'
#' Function to simulate a movement track under a Constrained Correlated Random
#' Walk model. Called as needed by \code{simulate_tracks}.
#'
#' Insert description
#' @inheritParams sim_bcrw
#' @param boundary_size Numeric. Length of the side of the bounding box (m).
#' @return Returns a data frame containing the coordinates of each location
#' along the simulated track and the associated simulation parameter values as
#' specified in \code{create_scenarios_ccrw}.
#' @export
sim_ccrw <- function(n_steps,
                     sl_par,
                     rho,
                     boundary_size,
                     start_loc = data.frame(x = 0, y = 0),
                     scenario_id = NA
                     ) {

  if (is.na(rho)) {stop("Value of rho is NA")}
  if (is.na(boundary_size)) {stop("Value of boundary_size is NA")}

  # Initialize output
  out <- start_loc

  # Draw a random angle to start with
  prev_angle <- runif(1, 0, 2 * pi)

  for(s in 2:(n_steps + 1)) {

    steps <- rweibull(1, sl_par[1], sl_par[2])
    angles <- sum(c(prev_angle,
                    as.numeric(circular::rwrappedcauchy(1,
                                                        circular::circular(0),
                                                        rho,
                                                        control.circular=list(units="radians")))))

    x <- out$x[s - 1] + cos(angles) * steps
    y <- out$y[s - 1] + sin(angles) * steps

    while(x < start_loc$x - boundary_size | y < start_loc$y - boundary_size |
          x > start_loc$x + boundary_size | y > start_loc$y + boundary_size) {
      steps <- rweibull(1, sl_par[1], sl_par[2])
      angles <- sum(c(prev_angle,
                      as.numeric(circular::rwrappedcauchy(1,
                                                          circular::circular(0),
                                                          rho,
                                                          control.circular=list(units="radians")))))
      x <- out$x[s - 1] + cos(angles) * steps
      y <- out$y[s - 1] + sin(angles) * steps

    }

    prev_angle <- angles
    out <- rbind(out, data.frame(x = x, y = y))
  }

  out$step <- 0:(nrow(out)-1)
  out$rho <- rho
  out$boundary_size <- boundary_size
  out$habitat_effect <- NA
  out$beta <- NA
  out$landscape <- NA
  out$scenario_id <- scenario_id

  return(out)

}
