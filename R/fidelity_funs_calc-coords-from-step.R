# Function to calculate end point coordinates from step and turning angle
# Code adapted from https://rdrr.io/github/ptompalski/UsefulRFunctions/src/R/circular2xy.R

end_coords <- function(step, # Step length (m)
                       angle, # Turning angle
                       start_x, # X coordinate of start location (UTM)
                       start_y # Y coordinate of start location (UTM)
                       ) {
  
  delta_x <- step * cos(angle)
  delta_y <- step * sin(angle)
  end_x <- start_x + delta_x
  end_y <- start_y + delta_y
  return(data.frame(x = end_x, y = end_y))
  
}
