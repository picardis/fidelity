#' Calculate Spatial Intensity of Returns (Multiple Tracks)
#'
#' Function to calculate spatial intensity of returns.
#'
#' @param rets Data frame of returns obtained from \code{calc_returns()}.
#' @param dist Numeric. Distance used to calculate returns. Unit should
#' match the unit of the coordinates: m if UTM, degrees if lat/long.
#' @param lag Numeric. Temporal lag of returns (unit is locations, e.g., 42 for
#' one week at 6h resolution). Must match one of the lags used in
#' \code{calc_returns()}.
#' @return A data frame with 2 columns, one for scenario ID and one for the
#' number of returns to each revisited location. For each scenario, the data
#' frame will include as many rows as the number of locations that were
#' revisited at least once.
#' @export

calc_spint <- function(rets,
                       dist,
                       lag) {

  scenarios <- sort(unique(rets$scenario_id))
  ids <- unique(rets$id)

  ppp_list <- list()

  ppp_list <- lapply(scenarios, function(s) {
    lapply(ids, function(i) {

      r <- dplyr::filter(rets, scenario_id == s & id == i)

      out_r <- calc_spint_track(r = r,
                                dist = dist,
                                lag = lag)

      return(out_r)

    })
  })

  ppp_list_c <- lapply(ppp_list, function(x) {

    rvals <- dplyr::bind_rows(lapply(x, function(i) {

      return(data.frame(vals = i))

    }))

    return(rvals)

  })

  names(ppp_list_c) <- scenarios

  ppp <- dplyr::bind_rows(ppp_list_c, .id = 'scenario_id')
  ppp <- ppp[!is.na(ppp$vals), ]

  return(ppp)

}

#' Calculate Spatial Intensity of Returns (Single Track)
#'
#' Function to calculate spatial intensity of returns on a single scenario/ID
#' combination. Called by \code{calc_spint()}.
#'
#' @param r Data frame of returns obtained from
#' \code{calc_returns()} for one individual.
#' @param dist Numeric. Distance used to calculate returns. Unit should
#' match the unit of the coordinates: m if UTM, degrees if lat/long.
#' #' @param lag Numeric. Temporal lag of returns (unit is locations, e.g., 42 for
#' one week at 6h resolution). Must match one of the lags used in
#' \code{calc_returns()}.
#' @return A vector with as many elements as the number of locations that were
#' revisited at least once. Values indicate the number of returns to each
#' of those revisited locations.
#' @export

calc_spint_track <- function(r,
                             dist,
                             lag) {

  # Filter locations that are returns at the chosen lag
  colname <- paste0("lag_", lag)
  r <- r[r[, colname] == 1 & !is.na(r[, colname]), ]

  # If no returns, return NA and move on
  if(nrow(r) == 0) {return(NA)} else {

  # Transform track into sf object
  r_sf <- sf::st_as_sf(r, coords = c("x", "y"))


  # Create raster with extent large enough to encompass all points and
  # resolution == the distance chosen for returns
  r <- raster::raster(xmn = floor(min(sf::st_coordinates(r_sf)[, 1]))-10,
              xmx = ceiling(max(sf::st_coordinates(r_sf)[, 1]))+10,
              ymn = floor(min(sf::st_coordinates(r_sf)[, 2]))-10,
              ymx = ceiling(max(sf::st_coordinates(r_sf)[, 2]))+10,
              res = dist)

  # Count how many points in each pixel
  out <- raster::rasterize(r_sf, r, field = 1, fun = "count")

  # Remove any NAs
  out <- raster::values(out)[!is.na(raster::values(out))]

  return(out)

}}
