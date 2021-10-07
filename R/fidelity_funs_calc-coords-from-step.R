#' Calculate End Point Coordinates
#'
#' Function to calculate end point coordinates from step and turning angle
#'
# Code adapted from https://rdrr.io/github/ptompalski/UsefulRFunctions/src/R/circular2xy.R

#' @param step Numeric. The step length in meters (m)
#' @param angle Numeric. The turning angle (degrees or radians?)
#' @param start_x Numeric. he X coordinate of the start location (UTM)
#' @param start_y Numeric. The Y coordinate of the start location (UTM)
#' @return A data frame containing the end points coordinates from the step and turning angle input.
#' @export

end_coords <- function(step,
                       angle,
                       start_x,
                       start_y
                       ) {

  delta_x <- step * cos(angle)
  delta_y <- step * sin(angle)
  end_x <- start_x + delta_x
  end_y <- start_y + delta_y
  return(data.frame(x = end_x, y = end_y))

}
