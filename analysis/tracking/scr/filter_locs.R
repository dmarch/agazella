#-------------------------------------------------------------------------------------
# BSH_02_postproc     Main processing of tracking data
#-------------------------------------------------------------------------------------
# This script processes animal tracking data following a common approach between
# different species.
#
# Main steps are:
# - Selection of tracks given a defined criteria
# - Filter location data: Near-duplicate positions, filter, angle and point on land


require("argosfilter")

#---------------------------------------------------------------
# Prepare cluster
#---------------------------------------------------------------
cl <- makeCluster(cores)
registerDoParallel(cl)


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

input_data <- paste0("data/out/tracking/", sp_code, "/L0_locations")
output_data <- paste0("data/out/tracking/", sp_code, "/L1_locations")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Select data
#---------------------------------------------------------------

# import all location files
loc_files <- list.files(input_data, full.names = TRUE, pattern = "L0_locations.csv")
df <- readTrack(loc_files)

# summarize data per trip
trips <- summarizeTrips(df)

# filter trips
trips <- filter(trips,
             duration_h >= sel_min_dur,
             n_loc >= sel_min_loc,
             distance_km >= sel_min_dist, 
             !id %in% sel_exclude)

tags <- unique(trips$id)

#---------------------------------------------------------------
# 3. Filter data for each track and trip
#---------------------------------------------------------------

foreach(i=tags, .packages=c("dplyr", "ggplot2", "gridExtra", "grid", "data.table", "argosfilter", "stringr")) %dopar% {

  print(paste("Processing tag", i))
  
  # subset data
  # filter by id and selected trips
  data <- filter(df, id == i, trip %in% trips$trip)
  
  # get list of trips to process
  trip_list <- unique(data$trip)

  data_list <- list()

  # filter by speed and near-duplicates at trip level
  for(j in 1:length(trip_list)){
    
    # subset data
    jtrip <- trip_list[j]
    sdata <- subset(data, trip == jtrip)
    
    # Filter out Z location classess
    sdata <- filter(sdata, lc != "Z")
    
    # Remove near-duplicate positions
    sdata <- filter_dup(sdata, step.time = filt_step_time, step.dist = filt_step_dist)
    
    # Filter speed for GPS
    if (tag_type == "GPS"){
      sdata <- filter_speed(sdata, vmax = (filt_vmax * 3.6), method = 1)
    }
    
    # Filter positions by speed and angle for PTT
    if (tag_type == "PTT"){
      sdata$argosfilter <- sdafilter(lat = sdata$lat, lon = sdata$lon, dtime = sdata$date, lc = sdata$lc,
                                     vmax = filt_vmax, # in m/s
                                     ang = filt_ang, # No spikes are removed if ang=-1
                                     distlim = filt_distlim)
      sdata <- filter(sdata, argosfilter == "not")
    }

    # Filter points on land
    if(filt_land == TRUE){
      sdata$onland <- point_on_land(lat = sdata$lat, lon = sdata$lon, land = land)
      sdata <- filter(sdata, onland == FALSE)
    }
    
    # append to list
    data_list[[j]] <- sdata
  }
  
  
  # Combine data from multiple trips
  dataL1 <- rbindlist(data_list)
  
  # Trim tracks into segments according to temporal gaps
  # This part only applies on Argos data
  if(tag_type == "PTT"){
    dataL1$trip <- timedif.segment(dataL1$date, thrs = trip_time_gap)
    dataL1$trip <- paste(i, str_pad(dataL1$trip, 3, pad = "0"), sep="_") 
    #dataL1$trip <- segmentOnPort(dataL1$habitat)  # generate segments including periods on land
    #dataL1 <- filter(dataL1, habitat == 2)  # remove periods on land
    #dataL1$trip <- segmentOnPort(dataL1$trip)  # regenerate trip id
    #dataL1$trip <- paste(i, str_pad(dataL1$trip, 3, pad = "0"), sep="_")
  }

  # store track data into individual folder at output path
  out_file <- paste0(output_data, "/", i, "_L1_locations.csv")
  write.csv(dataL1, out_file, row.names = FALSE)
  
  # plot track
  p1 <- mapL1(dataL1) 
  
  # plot histo
  if(tag_type == "PTT") p2 <- diffTimeHisto(dataL1, vline=2)
  if(tag_type == "GPS") p2 <- diffTimeHistoGPS(dataL1, vline=0.08)
  
  # combine plots
  lay <- rbind(c(1,1),
               c(1,1),
               c(2,2))
               
  p <- grid.arrange(p1, p2, layout_matrix = lay)
  
  # export multi-panel plot
  out_file <- paste0(output_data, "/", i, "_L1_locations.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")
}


#---------------------------------------------------------------
# 4. Summarize processed data
#---------------------------------------------------------------

# import all location files
loc_files <- list.files(output_data, full.names = TRUE, pattern = "L1_locations.csv")
df <- readTrack(loc_files)

# summarize data per animal id
idstats <- summarizeId(df)

# export table
out_file <- paste0(output_data, "/", sp_code, "_summary_id.csv")
write.csv(idstats, out_file, row.names = FALSE)


#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)


print("Filtering ready")