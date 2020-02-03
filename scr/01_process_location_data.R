#----------------------------------------------------------------------------------------------
# 01_process_location_data.R             Process location data from satellite tags
#----------------------------------------------------------------------------------------------


# Load dependencies
source("scr/fun/fun_data_processing.R")


# Prepare data
file <- "data/TelemetriaPPT.xlsx"
df <- read_agazella(file)  # takes time
write.csv(df, "temp/all_l0_tracks.csv", row.names=FALSE)  # export temporal file with all tracks
table(df$ptt)


# Filter and restructure data
ids <- unique(df$ptt)
for(i in ids){
  
  # subset data for given tag
  dataL0 <- subset(df, ptt == i)
  
  # filter locations
  data_sdl <- L02L1_sdl(data = dataL0, vmax = 50, step.time = 5/60, step.dist = 0.001)  # speed is in km/h. Max speed for pinniped is 8 m/s (Ray 1963)
  dataL1 <- data_sdl$data
  
  # export processed data
  csvL1 <- paste0("out/proc/", i, "_L1_locations.csv")
  write.csv(dataL1, csvL1, row.names = FALSE)
}

# combine multiple tracks (L1)
pat <- paste0("L1_locations.csv")
loc_file <- list.files("out/proc/", full.names=TRUE, recursive = TRUE, pattern=pat)
all_l1_tracks <- readTrack(loc_file)
write.csv(all_l1_tracks, "temp/all_l1_tracks.csv", row.names=FALSE)


