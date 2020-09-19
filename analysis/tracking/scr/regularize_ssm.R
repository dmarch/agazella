#-------------------------------------------------------------------------------------
# 03_regularize_foieGras     Interpolate tracks into regular time steps using foieGras
#-------------------------------------------------------------------------------------
# This script processes animal tracking data following a common approach between
# different species.
#
# Main steps are:
# - Regularize tracks

#devtools::install_github("ianjonsen/foieGras")  # github version fixes problem with GPS
#library("foieGras")


#---------------------------------------------------------------
# Prepare cluster
#---------------------------------------------------------------
cl <- makeCluster(cores)
registerDoParallel(cl)

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

input_data <- paste0("data/out/tracking/", sp_code, "/L1_locations")
output_data <- paste0("data/out/tracking/", sp_code, "/L2_locations")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Import data
#---------------------------------------------------------------

# import all location files
loc_files <- list.files(input_data, full.names = TRUE, pattern = "L1_locations.csv")
df <- readTrack(loc_files)


#---------------------------------------------------------------
# 2. Select trips to run the SSM
#---------------------------------------------------------------

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
# 3. Regularize each track using a SSM
#---------------------------------------------------------------

foreach(i=tags, .packages=c("dplyr", "ggplot2", "foieGras", "stringr")) %dopar% {
#for (i in tags){
  
  print(paste("Processing tag", i))
  # 
  # # import data
  # loc_file <- paste0(input_data, "/", i, "_L1_locations.csv")
  # data <- readTrack(loc_file)
  
  # subset data
  # filter by id and selected trips
  data <- filter(df, id == i, trip %in% trips$trip)

  ###### State-Space Model
  
  # convert to foieGras format
  if(tag_type == "GPS") indata <- data %>% dplyr::select(trip, date, lc, lon, lat) %>% rename(id = trip) 
  if(tag_type == "PTT") indata <- data %>% dplyr::select(trip, date, lc, lon, lat, smaj, smin, eor) %>% rename(id = trip) 
  
  # filter location class data with NA values
  # very few cases, but creates an error in fit_ssm
  indata <- dplyr::filter(indata, !is.na(lc))
  
  # fit SSM
  # we turn sdafilter off because we previously filtered data
  # we run the model with multiple trips at once
  fit <- fit_ssm(indata, model = "crw", time.step = reg_time_step, verbose = 0, spdf = FALSE)
  
  # get fitted locations
  # segments that did not converge were not consider
  data <- data.frame(grab(fit, what = "predicted", as_sf = FALSE))
  data <- data %>% rename(trip = id) %>% arrange(date)
  data <- cbind(id = i, data)
  
  # check if points on land
  #data$onland <- point_on_land(lat = data$lat, lon = data$lon, land = land)
  
  # export track data into individual folder at output path
  out_file <- paste0(output_data, "/", i, "_L2_locations.csv")
  write.csv(data, out_file, row.names = FALSE)
  
  # export convergence status
  convergence <- data.frame(id = i, trip = fit$id, converged = fit$converged)
  out_file <- paste0(output_data, "/", i, "_L2_convergence.csv")
  write.csv(convergence, out_file, row.names = FALSE)
  
  # plot figures
  # plot(fit, what = "predicted", type = 1)
  # plot(fit, what = "fitted", type = 2)
  # fmap(fit, what = "predicted", obs = FALSE)
  p <- mapL1(data = data)
  out_file <- paste0(output_data, "/", i, "_L2_locations.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")
}
  
stopCluster(cl) # Stop cluster

#---------------------------------------------------------------
# 4. Summarize processed data
#---------------------------------------------------------------

# import all location files
loc_files <- list.files(output_data, full.names = TRUE, pattern = "L2_locations.csv")
df <- readTrack(loc_files)

# import convergence files
loc_files <- list.files(output_data, full.names = TRUE, pattern = "L2_convergence.csv")
data_proc <- lapply(loc_files, read.csv) %>% rbindlist

# summarize data per trip
tripstats <- summarizeTrips(df)

# combine track data summary and convergence status
comb <- merge(tripstats, data_proc, by=c("id", "trip"))

# export table
out_file <- paste0(output_data, "/", sp_code, "_summary_ssm.csv")
write.csv(comb, out_file, row.names = FALSE)


print("Regularization ready")