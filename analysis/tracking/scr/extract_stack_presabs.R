#-------------------------------------------------------------------------------------
# extract_stack_presabs        Extract environment data from presence absence
#-------------------------------------------------------------------------------------
# This script extracts environmental data



#---------------------------------------------------------------
# Prepare cluster
#---------------------------------------------------------------
cl <- makeCluster(cores)
registerDoParallel(cl)


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

# set data paths
indir <- paste0(output_data, "/tracking/", sp_code, "/PresAbs")
outdir <- indir
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


#---------------------------------------------------------------
# 3. Import presence/absence data
#---------------------------------------------------------------

# import observations
obs_file <- outfile <- paste0(indir, "/", sp_code, "_L2_PresAbs.csv")
data <- read.csv(obs_file)
data$date <- ymd(data$date)


#---------------------------------------------------------------
# 4. Extract habitat
#---------------------------------------------------------------

# filter out data after max date
# This is limited to the collected environmental repository
data <- filter(data, date < env_max_date)

# extract environmental data from stack
enviro <- rbindlist(foreach(j=1:nrow(data), .packages = c("lubridate", "raster", "stringr"))  %dopar% {
  extract_stack(date = data$date[j], lon = data$lon[j], lat = data$lat[j], buffer = env_buffer, repo = stack_repo)
})

# combine
data <- cbind(data, enviro)

# select
data <- dplyr::select(data, sp_code, date, lon, lat, occ, all_of(all_vars))

# remove grid cells with > 1 variable with missing data
data$na_count <- rowSums(is.na(data))
data <- data[data$na_count <= 1,]

# export
outfile <- paste0(outdir, "/", sp_code, "_observations.csv")
write.csv(data, outfile, row.names = FALSE)


#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("Environmental data extracted ready")  