#--------------------------------------------------------------------------------
# 02_animated_maps.R          Visualize and animate tracks
#--------------------------------------------------------------------------------


library(move)
library(moveVis)


# Select location data files
pat <- paste0("L1_locations.csv")
loc_file <- list.files("out/proc/", full.names=TRUE, recursive = TRUE, pattern=pat)



#------------------------------------------------------------------------
# Make a map with all tracks
#------------------------------------------------------------------------

# import location data
dataL1 <- readTrack(loc_file)

# rename data.frame of tracks
df <- data.frame(dataL1)

# convert to move object
mdata <- move(x=df$lon, y=df$lat, time=df$date, 
              data=df, proj=CRS("+proj=longlat +ellps=WGS84"), 
              animal=df$ptt)

# align move_data to a uniform time scale
# for animation, unique frame times are needed 
m <- align_move(mdata, res = 0.5, digit = 0, unit = "days", spaceMethod = "greatcircle")

# create spatial frames with an OpenStreetMap watercolour map
frames <- frames_spatial(m,  # move object
                         map_service = "carto", map_type = "voyager_no_labels",  # base map
                         path_size = 2, alpha = 0.5) %>%  # path
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations
  add_scalebar(colour = "black", position = "bottomleft") %>% 
  add_timestamps(m, type = "label") %>% 
  add_progress(size = 2)

# animate frames and export them
filename <- paste0("out/animation/all_L1_track.mp4")
animate_frames(frames, out_file = filename, overwrite = TRUE, display = FALSE)



#------------------------------------------------------------------------
# Make a map for each individual
#------------------------------------------------------------------------

# Process data per file
for(i in loc_file){
  
  # import location data
  dataL1 <- readTrack(i)
  ptt <- dataL1$ptt[1]
  
  # rename data.frame of tracks
  df <- data.frame(dataL1)
  
  # convert to move object
  mdata <- move(x=df$lon, y=df$lat, time=df$date, 
                data=df, proj=CRS("+proj=longlat +ellps=WGS84"), 
                animal=df$ptt)
  
  # align move_data to a uniform time scale
  # for animation, unique frame times are needed 
  m <- align_move(mdata, res = 0.5, digit = 0, unit = "days", spaceMethod = "greatcircle")
  
  # create spatial frames with an OpenStreetMap watercolour map
  frames <- frames_spatial(m,  # move object
                           map_service = "carto", map_type = "voyager_no_labels",  # base map
                           path_size = 2, alpha = 0.5) %>%  # path
    add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations
    add_scalebar(colour = "black", position = "bottomleft") %>% 
    add_timestamps(m, type = "label") %>% 
    add_progress(size = 2)

  # animate frames and export them
  filename <- paste0("out/animation/", ptt, "_track.mp4")
  animate_frames(frames, out_file = filename, overwrite = TRUE, display = FALSE)
}
