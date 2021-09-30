#' Simulate Constrained Correlated Random Walk (CCRW)
#'
#' Description
#' @inheritParams sim_crw
#' @param boundary_size Size of the boundary in m. Numeric value
sim_ccrw <- function(n_steps,
                     sl_par,
                     rho,
                     boundary_size,
                     start_loc = data.frame(x = 0, y = 0),
                     scenario_id = NA
                     ) {

  # Initialize output
  out <- start_loc

  # Draw a random angle to start with
  prev_angle <- runif(1, 0, 2 * pi)

  for(s in 2:(n_steps + 2)) {

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

  out$step <- 0:(n_steps + 1)
  out$rho <- rho
  out$boundary_size <- boundary_size
  out$habitat_effect <- NA
  out$beta <- NA
  out$landscape <- NA
  out$scenario_id <- scenario_id

  return(out)

}
