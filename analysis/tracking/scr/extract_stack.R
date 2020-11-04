#-------------------------------------------------------------------------------------
# extract_stack        Extract environment data from stack
#-------------------------------------------------------------------------------------
# This script extracts environmental data
#
# Main steps are:
# - Extract static data
# - Extract dynamic data



#---------------------------------------------------------------
# Prepare cluster
#---------------------------------------------------------------
cl <- makeCluster(cores)
registerDoParallel(cl)


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

# set data paths
input_data <- paste0("data/out/tracking/", sp_code, "/", input_folder)
output_data <- paste0("data/out/tracking/", sp_code, "/", output_folder)
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 3. Select data
#---------------------------------------------------------------

# List files from input data
loc_files <- list.files(input_data, full.names = TRUE, pattern="L2_locations.csv")


#---------------------------------------------------------------
# 4. Extract habitat
#---------------------------------------------------------------

for (i in 1:length(loc_files)){
  
  print(paste("Processing tag", i))
  
  #------------------------------------------------
  # Combine observations and simulations
  #------------------------------------------------
  
  # import observations
  data <- readTrack(loc_files[i])
  
  # filter out data after max date
  # This is limited to the collected environmental repository
  data <- filter(data, date < env_max_date)
  if(nrow(data)==0) next

  # extract environmental data from stack
  enviro <- rbindlist(foreach(j=1:nrow(data), .packages = c("lubridate", "raster", "stringr"))  %dopar% {
    extract_stack(date = data$date[j], lon = data$lon[j], lat = data$lat[j], buffer = env_buffer, repo = "data/out/environment/stack_daily")
  })
  
  # combine
  data <- cbind(data, enviro)
  
  # export track data into individual folder at output path
  id <- data$id[1]
  out_file <- paste0(output_data, "/", id, "_L3_locations.csv")
  write.csv(data, out_file, row.names = FALSE)
  
  #------------------------------------------------
  # Plot time series of environmental data
  #------------------------------------------------
  if(plotTS == TRUE){
    p <- plotEnviroTS(data, vars = names(enviro))
    out_file <- paste0(output_data, "/", id, "_L3_locations.png")
    ggsave(out_file, p, width=30, height=40, units = "cm")
  }

}


#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("Environmental data extracted ready")  