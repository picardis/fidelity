# Set seed ####

set.seed(1)

# Neighbors ####

data(lands)

prange <- 50
neigh <- get_neighbors(lands[[1]], prange = prange)

usethis::use_data(neigh, overwrite = TRUE)

# Simulations ####

prange <- 50

scen_crw <- create_scenarios_crw(rho = c(0, 0.5, 0.99))

land_names <- paste0(getwd(),
                     "/inst/Landscapes/land_",
                     names(lands),
                     ".tif")

scen_bcrw <- create_scenarios_bcrw(rho = 0.5,
                                   beta = c(0, 0.1, 0.5),
                                   lands = land_names)

scen <- rbind(scen_crw, scen_bcrw)

data(neigh)

sim <- simulate_tracks(scenarios = scen,
                       n_steps = 100,
                       n_tracks = 1,
                       start_loc = data.frame(x = 0, y = 0),
                       sl_par = c(1, 20),
                       prange = prange,
                       jitter = FALSE,
                       neighbors = neigh)

sim2 <- sim

# Remove personal info
sim$landscape <- stringr::word(sim$landscape, 6, 9, "/")

usethis::use_data(sim, overwrite = TRUE)

# Returns ####

sim_list <- split(sim2, f = paste(sim2$id, sim2$scenario_id))

rets <- do.call("rbind", lapply(1:length(sim_list),
                                FUN = function(x) {
                                  fidelity::calc_returns(
                                    tracks = sim_list[[x]],
                                    dist = 10,
                                    lag = 10)
                                }))

usethis::use_data(rets, overwrite = TRUE)

# Mule deer ####

mule <- readRDS("C:/Users/A02343179/Box Sync/Manuscripts/2022 - Site Fidelity Merkle Lab/fidelity/data/Empirical_iSSF/data_GPS/muledeer_summer2016.rds")
mule2 <- sf::st_as_sf(mule, coords = c("X", "Y"), crs = 4326)
mule2 <- sf::st_transform(mule2, crs = 32612)
mule3 <- cbind(mule[, c("id", "date")], sf::st_coordinates(mule2))
mule <- mule3
colnames(mule) <- c("id", "date", "x", "y")

usethis::use_data(mule, overwrite = TRUE)
