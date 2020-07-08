#-------------------------------------------------------------------------------------
# 04_simulations        Simulate tracks
#-------------------------------------------------------------------------------------
# This script simulates tracks to generate pseudo absences
#
# Main steps are:
# - Create an ocean mask
# - Simulate tracks


# TODO: Add time-varying land mask (e.g. sea ice)

library(availability)


#---------------------------------------------------------------
# Prepare cluster
#---------------------------------------------------------------
cl <- makeCluster(cores)
registerDoParallel(cl)


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

input_data <- paste0("data/out/tracking/", sp_code, "/L2_locations")
output_data <- paste0("data/out/tracking/", sp_code, "/L2_simulations")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Create oceanmask
#---------------------------------------------------------------

# create base raster
r <- raster(extent(ocean) + 0.5, res = sim_mask_res, crs = crs(ocean))

# rasterize ocean mask
rocean <- rasterize(as(ocean,"SpatialPolygons"), r)

# remove gibraltar strait
# in preliminar simulations it has cul-de-sac effect
e <- extent(ocean)+c(0.5,0,0,0)
rocean <- crop(rocean, e)

# Define resistance values (0 = ocean, 1 = land)
rocean[rocean==1] <- 0
rocean[is.na(rocean)] <- 1
oceanmask <- rocean

#---------------------------------------------------------------
# 3. Select data
#---------------------------------------------------------------

# import summary data
db <- read.csv(paste0(input_data, "/", sp_code, "_summary_ssm.csv"))

# select by number of locations and minimum duration
tags <- unique(db$id)
if (!is.null(sim_exclude)) tags <- tags[which(!tags %in% sim_exclude)]


#---------------------------------------------------------------
# 4. Simulate track
#---------------------------------------------------------------

foreach(i=tags, .packages=c("dplyr", "ggplot2", "availability", "data.table", "raster", "stringr")) %dopar% {
  #for (i in tags){
  
  print(paste("Processing tag", i))
  
  # import data
  loc_file <- paste0(input_data, "/", i, "_L2_locations.csv")
  data <- readTrack(loc_file)
  
  # select trips that converged in the SSM
  trips <- unique(data$trip)
  sel <- which(trips %in% db$trip[db$converged==TRUE])
  trips <- trips[sel]

  trip_list <- list()
  
  
  for (j in 1:length(trips)) {
    
    # select data for selected segment
    d <- dplyr::filter(data, trip == trips[j])
    
    # Fit a vector-autoregressive movement model to this filtered track.
    # This model assumes that the x- and y-speeds at time t are a linear function
    # of the speeds at time t-1, plus random noise.
    arfit <- surrogateARModel(d[,c("lon", "lat")])
    
    # Now we can use that fitted model to generate new tracks. 
    # Simulated track are fixed to the same start, but no end points
    # Land mask is applied:
    
    ## generate simulations
    data_list <- list()
    
    for (s in 1:sim_n){
      
      # Now we can use that fitted model to generate new tracks. 
      # Simulated track are fixed to the same start always. End points fixed for central-place foragers
      # Land mask is applied:
      simu <- surrogateAR(arfit, xs = d[,c("lon", "lat")], ts = d[,c("date")], point.check = landMask,
                          fixed = rep(c(TRUE, FALSE, sim_fix_last), c(1, nrow(d) - 2, 1)),
                          partial=FALSE)
      if(is.null(simu) | is.na(simu$xs[1])) break

      ## convert to our generic format for tracks
      sim_code <- str_pad(s, 3, pad = "0")
      df <- data.frame(id = i, trip = trips[j], nsim = s, simid = paste(trips[j], sim_code, sep="_"), date = simu$ts, lon = simu$xs[,1], lat = simu$xs[,2])
      
      ## append data.frame into list
      data_list[[s]] <- df
    }
    
    ## combine simulations into a single data.frame
    simdf <- rbindlist(data_list)
    
    ## append to segment list
    trip_list[[j]] <- simdf
  }
  
  ## combine simulations into a single data.frame
  simdf <- rbindlist(trip_list)
  
  # export track data into individual folder at output path
  out_file <- paste0(output_data, "/", i, "_sim_L2_locations.csv")
  write.csv(simdf, out_file, row.names = FALSE)
  
  ## Plot simulations
  p <- mapSimTracks(simData = simdf, obsData = data, title = paste("ID", i))
  out_file <- paste0(output_data, "/", i, "_sim_L2_locations.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")
}
  

#---------------------------------------------------------------
# 5. Summarize processed data
#---------------------------------------------------------------

# identify all location files
loc_files <- list.files(output_data, full.names = TRUE, pattern = "sim_L2_locations.csv")

# sumarize number of simulations per trip
# some trips may have problems for simulation. So, next steps will filter trip
# data that has been simulated successfuly.
sim_stats <- rbindlist(foreach(i=loc_files, .packages=c("dplyr", "data.table")) %dopar% {
  df <- readTrack(i)
  simdf <- summarizeSim(df)
  return(simdf)
})

# combine with trip data from the SSM (input data)
comb <- merge(db, sim_stats, by="trip", all.x=TRUE)


# export table
out_file <- paste0(output_data, "/", sp_code, "_summary_sim.csv")
write.csv(comb, out_file, row.names = FALSE)


  
#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)


print("Simulations ready")        