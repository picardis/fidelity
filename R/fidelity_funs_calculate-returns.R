#' Calculate Returns (Multiple)
#'
#' \code{calc_returns} Function to calculate returns on multiple tracks based on temporal and spatial restrictions.
#'
#' @param tracks Data frame of movement tracks (e.g., output from simulate_tracks(), or a set of real tracks) with at minimum columns for id, x, y, and step.
#' @param dist Numeric. Maximum distance (UNIT...m?) to be considered a return
#' @param lag Vector of fidelity lags (one or more)
#' @param window # Duration of the fidelity window (Inf if from the start of the track, an integer if shorter (e.g., 42 locations, which is a week at 6h resolution). Default is Inf.
#' @return Data frame of movement tracks containing an additional column indicating a return (1 = return, 0 = no return) for each lag.
#' @export

calc_returns <- function(tracks,
                         dist,
                         lag,
                         window = Inf
                         ) {

  # Check if tracks is the output of simulate_tracks()
  if (!is.null(tracks$scenario_id)) {
    track_list <- split(tracks, f = paste(tracks$id, tracks$scenario_id))
  } else if (is.null(tracks$scenario_id)) {
    track_list <- split(tracks, f = tracks$id)
    message("Looks like these are real (not simulated) tracks.
            scenario_id is missing, splitting by id")
  }

  ret <- lapply(track_list, function(t) {calc_ret_track(t = t,
                                                        dist = dist,
                                                        lag = lag,
                                                        window = window)})

  out <- do.call("rbind", ret)

  rownames(out) <- NULL

  return(out)

}


#' Calculate Returns (Single)
#'
#' \code{calc_ret_track} Function to calculate returns for a single track based on temporal and spatial restrictions.
#'
#' @param tracks Data frame of a movement track (e.g., output from simulate_tracks(), or a set of real tracks) with at minimum columns for id, x, y, and step.
#' @inheritParams calc_returns
#' @return Data frame of a movement track containing an additional column indicating a return (1 = return, 0 = no return) for each lag.
#' @export

calc_ret_track <- function(t,
                           dist,
                           lag,
                           window = Inf) {

  n_steps <- nrow(t) - 1

  distmat <- fields::rdist(t[, c("x", "y")])

  ret <- do.call(cbind, lapply(lag, function(l) {

    # Initialize vector of results
    rev <- rep(NA, n_steps)

    # Check that fidelity window doesn't start after end of steps
    if (n_steps > (l + 1)) {
      # Loop over steps
      for (s in (l + 1):n_steps) {

        # Identify start and end of fidelity window for current step
        end_fw <- s - l
        start_fw <- max(1, end_fw - window)

        # Indices of steps within fidelity window
        fw <- start_fw:end_fw

        # Is this a return?
        rev[s] <- any(distmat[s, fw] <= dist)

      }
    }

    rev <- as.numeric(c(NA, rev))

    return(rev)

  }))

  colnames(ret) <- paste0("lag_", lag)

  out <- cbind.data.frame(t, ret)

  return(out)

}
