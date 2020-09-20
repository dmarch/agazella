#-------------------------------------------------------------------------------------
# animate_sim        Animate simulations
#-------------------------------------------------------------------------------------
# This script plots animated simulations together with observed track




#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
input_data_obs <- paste0("data/out/tracking/", sp_code, "/L2_locations")
input_data_sim <- paste0("data/out/tracking/", sp_code, "/L2_simulations")
output_data <- paste0("data/out/tracking/", sp_code, "/L2_animations")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

#---------------------------------------------------------------
# Prepare cluster
#---------------------------------------------------------------
# define number of cores manually
use_multicore(n_cores = 6)

#---------------------------------------------------------------
# 2. Select data
#---------------------------------------------------------------

# import summary data
db <- read.csv(paste0(input_data_obs, "/", sp_code, "_summary_ssm.csv"))

# select by number of locations and minimum duration
tags <- unique(db$id)
if (!is.null(sim_exclude)) tags <- tags[which(!tags %in% sim_exclude)]


#---------------------------------------------------------------
# 3. Animate
#---------------------------------------------------------------

for (i in tags){

  print(paste("Processing tag", i))

  # import observed data
  loc_file <- paste0(input_data_obs, "/", i, "_L2_locations.csv")
  data <- readTrack(loc_file) %>% data.frame()
  data$colour <- "red" 

  # import simulated data
  loc_file <- paste0(input_data_sim, "/", i, "_sim_L2_locations.csv")
  simdf <- readTrack(loc_file)  %>% data.frame()
  simdf$colour <- "blue" 

  # combine observed and simulated
  data$nsim <- 0  # create dummy simulation number
  data <- dplyr::select(data, lon, lat, date, nsim, colour)  # select minimal data
  simdf <- dplyr::select(simdf, lon, lat, date, nsim, colour)  # select minimal data
  df  <- rbind(data, simdf)  # combine datasets into a data.frame
  df <- arrange(df, nsim, date)  # group data by individual
  
  # convert to move object
  mdata <- move(x=df$lon, y=df$lat, time=df$date, 
                data=df, proj=CRS("+proj=longlat +ellps=WGS84"), 
                animal=df$nsim)
  
  # align move_data to a uniform time scale
  # for animation, unique frame times are needed 
  m <- align_move(mdata, res = 2, digit = 0, unit = "hours", spaceMethod = "greatcircle")
  
  # create spatial frames with an OpenStreetMap watercolour map
  frames <- frames_spatial(m,  # move object
                           map_service = "carto", map_type = "voyager_no_labels",  # base map
                           path_size = 1,
                           alpha = 0.5) %>%  # path
    add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations
    add_scalebar(colour = "black", position = "bottomright") %>% 
    add_timestamps(m, type = "label") %>% 
    add_progress(size = 2)
  
  # check frame
  #frames[[80]]
  
  # animate frames and export them
  out_file <- paste0(output_data, "/", i, "_sim_L2_animation.mp4")
  animate_frames(frames, out_file = out_file, overwrite = TRUE, display = FALSE)
}

