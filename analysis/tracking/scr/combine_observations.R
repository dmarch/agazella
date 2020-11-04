#-------------------------------------------------------------------------------------
# combine_observations       Create presence/absence matrix for habitat model
#-------------------------------------------------------------------------------------
# This script combines observed and simulate tracks to create a presence/absence
# table that will be used for analysis of habitat modeling
#
# Main steps are:
# combines presence/absence
# select fields (consider those to identify different stages/colonies if needed)
# select number of simulations



#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
# Output folder is common for all tracking species

ssm_data <- paste0("data/out/tracking/", sp_code, "/L3_locations")
sim_data <- paste0("data/out/tracking/", sp_code, "/L3_simulations")
output_data <- "data/out/habitat_model/observations"
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Presence data
#---------------------------------------------------------------

# Presence data (observed state-space models)
loc_files <- list.files(ssm_data, full.names = TRUE, pattern="L3_locations.csv")
obs <- readTrack(loc_files)

## Add occurence column
obs$occ <- 1  # presence

# select fields
obs <- dplyr::select(obs, id, trip, date, lon, lat, occ, all_vars)



#---------------------------------------------------------------
# 3. Pseudo-absence data
#---------------------------------------------------------------

# Simulations
loc_files <- list.files(sim_data, full.names = TRUE, pattern="L3_locations.csv")
sim <- readTrack(loc_files)

# Filter data by number of simulations
sim <- filter(sim, nsim <= sim_n)

## Add occurence column
sim$occ <- 0  # presence

# select fields
sim <- dplyr::select(sim, id, trip, date, lon, lat, occ, all_vars)


#---------------------------------------------------------------
# 4. Check that all trips are present in both presence and absence
#---------------------------------------------------------------
# There may be some SSM tracks that have not been processed in simulations
# (e.g. animals that left the study area). We filter out these tracks from further
# analysis

simTrips <- unique(sim$trip)
obs <- dplyr::filter(obs, trip %in% simTrips)



#---------------------------------------------------------------
# 5. Combine observations and keep for habiat modeling
#---------------------------------------------------------------

df <- rbind(obs, sim)
out_file <- paste0(output_data, "/", sp_code, "_observations.csv")
write.csv(df, out_file, row.names = FALSE)