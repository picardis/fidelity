#' Calculate Returns (Multiple Tracks)
#'
#' Function to calculate returns on multiple tracks based on spatial and
#' temporal criteria.
#'
#' @param tracks Data frame of movement tracks (e.g., output from
#' \code{simulate_tracks()}, or a set of real tracks). Must contain columns
#' \code{id} (individual identifier), \code{x} and \code{y} (UTM or lat/long
#' coordinates) at a minimum.
#' @param dist Numeric. Maximum distance to be considered a return. Unit should
#' match the unit of the coordinates: m if UTM, degrees if lat/long.
#' @param lag Numeric. Vector of fidelity lags (unit is locations, e.g., 42 for
#' one week at 6h resolution). See Details for definition of fidelity lag.
#' @param window # Duration of the fidelity window (\code{Inf} if from the start
#' of the track, an integer if fixed duration; unit is locations, e.g., 42 for
#' one week at 6h resolution). Default is \code{Inf}. See Details for definition
#' of fidelity window.
#' @return Data frame of movement tracks containing one additional column for
#' each lag indicating whether the location is a return (1) or not (0).
#' Locations outside the fidelity window are assigned \code{NA}.
#' @details The fidelity lag defines what are considered "non-consecutive"
#' visits: a visit within distance \code{dist} of a previously visited location
#' is non-consecutive if it occurs at least \code{lag} days after the latest
#' visit at that location.
#'
#' The fidelity window encompasses the range of eligible steps for returns to a
#' location. For a location visited at time T, the fidelity window includes
#' steps from time T - \code{lag} - \code{window} to time T - \code{lag}.
#' Steps from T - \code{lag} + 1 to T are outside the fidelity window and any
#' revisitations within that period are not counted as returns.
#' @export

calc_returns <- function(tracks,
                         dist,
                         lag,
                         window = Inf
                         ) {

  # Check columns
  if (is.null(tracks$id)) {stop("Column 'id' is missing")}
  if (is.null(tracks$x)) {stop("Column 'x' is missing")}
  if (is.null(tracks$y)) {stop("Column 'y' is missing")}

  # Check if tracks is the output of simulate_tracks()
  if (!is.null(tracks$scenario_id)) {
    track_list <- split(tracks, f = paste(tracks$id, tracks$scenario_id))
  } else if (is.null(tracks$scenario_id)) {
    track_list <- split(tracks, f = tracks$id)
    message("Column 'scenario_id' is missing, splitting by 'id' only")
  }

  ret <- lapply(track_list, function(t) {calc_ret_track(t = t,
                                                        dist = dist,
                                                        lag = lag,
                                                        window = window)})

  out <- do.call("rbind", ret)

  rownames(out) <- NULL

  return(out)

}


#' Calculate Returns (Single Track)
#'
#' Function to calculate returns for a single track. Called by
#' \code{calc_returns()}.
#'
#' @param t Data frame of a single movement track (e.g., output from
#' \code{simulate_tracks()} for one individual, or a real track). Must contain
#' columns \code{x} and \code{y} (UTM or lat/long coordinates) at a minimum.
#' @inheritParams calc_returns
#' @return Returns the original track with one additional column for each lag
#' indicating whether the location is a return (1) or not (0). Locations outside
#' the fidelity window are assigned \code{NA}.
#' #' @details The fidelity lag defines what are considered "non-consecutive"
#' visits: a visit within distance \code{dist} of a previously visited location
#' is non-consecutive if it occurs at least \code{lag} days after the latest
#' visit at that location.
#'
#' The fidelity window encompasses the range of eligible steps for returns to a
#' location. For a location visited at time T, the fidelity window includes
#' steps from time T - \code{lag} - \code{window} to time T - \code{lag}.
#' Steps from T - \code{lag} + 1 to T are outside the fidelity window and any
#' revisitations within that period are not counted as returns.
#' @export

calc_ret_track <- function(t,
                           dist,
                           lag,
                           window = Inf) {

  # Check columns
  if (is.null(t$x)) {stop("Column 'x' is missing")}
  if (is.null(t$y)) {stop("Column 'y' is missing")}

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

        # Is this location within the threshold distance of any locations
        # visited during the fidelity window?
        rev[s] <- any(distmat[which(distmat[s, fw] <= dist),
        # Had the animal left any of those locations during the residency window?
                  (end_fw + 1):(s - 1)] > dist)

      }
    }

    rev <- as.numeric(c(NA, rev))

    return(rev)

  }))

  colnames(ret) <- paste0("lag_", lag)

  out <- cbind.data.frame(t, ret)

  return(out)

}
