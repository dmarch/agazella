#----------------------------------------------------------------------------------
# GAZ_main.R            Main script for processing Anctartick fur seal tracking
#----------------------------------------------------------------------------------



#---------------------------------------------------------------
# 1. Set parameters
#---------------------------------------------------------------
sp_code <- "GAZ"  # species code
tag_type <- "PTT"


# Trip definition
trip_time_gap <- 7 * 24  # Tracks with data gaps in excess of [seg_time_gap] hours were broken up for separate modeling

# Track selection
sel_min_loc <- 20  # minimum number of locations
sel_min_dur <- 12 # minimum durantion of track, in hours
sel_exclude <- NULL # custom selection of tags based on exploration of data
sel_min_dist <- 15

# Track filtering
filt_step_time <- 2/60  # time difference to consider duplicated positions, in hours
filt_step_dist <- 1/1000  # spatial distance to consider duplicated poisitions, in km
filt_land <- FALSE  # remove locations on land
filt_vmax <- 10  # value of the maximum of velocity using in sdafilter in m/s
filt_ang <- c(15, 25) # value of the angle using in sdafilter, no spikes are removed if ang=-1
filt_distlim <- c(2500, 5000) # value of the limite distance using in sdafilter, no spikes are removed if ang=-1

# Track regularization
reg_time_step <- 2  # time step to interpolate positions, in hours

# Simulations
sim_mask_res <- 0.01  # resolution of ocean mask, in degrees
sim_n <- 20  # number of simulations
sim_fix_last <- FALSE  # fix last track location
sim_exclude <- c(50722, 50732, 50735, 50737, 95590, 151934) # these turtles left the study area

# Extract environment
env_buffer <- 15000  # radius of buffer to average environmental data around each location, in meters.
all_vars <- c("BAT", "SLP", "SDIST", "SMTD", "CANDIST", "SST", "SSTg", "SAL", "SALg", "SLA", "EKE", "CHL", "WIND", "WAV", "MLD")
env_max_date <- as.Date("2019-01-01")


#---------------------------------------------------------------
# 2. Set data paths and import libraries
#---------------------------------------------------------------

# Load dependencies
source("conf/config.R")
source("scr/fun_track_reading.R")  # read multiple tracking data formats
source("scr/fun_track_plot.R")  # plot tracking data
source("scr/fun_track_proc.R")  # miscellanea of processing functions
source("scr/fun_enviro.R")  # function to process environmental data


#---------------------------------------------------------------
# 3. Import external data
#---------------------------------------------------------------

# # Landmask
# land <- readOGR("data/raw/ext/landmask","landmask_med")
# land <- spTransform(land, crs_proj)
# 
# # Oceanmask
# ocean <- readOGR("data/raw/ext/oceanmask","WestMed_area")
# ocean <- spTransform(ocean, crs_proj)


#---------------------------------------------------------------
# 4. Processing workflow
#---------------------------------------------------------------

# Set number of cores for parallel processing
cores <- detectCores()-2

# Step 1. Pre-process data and standardize data
# Transforms different data sources into a common format
# Output data can be found: main_dir/output/species/sp_code/L0_locations
source("analysis/tracking/scr/preproc_GAZ.R")

# Step 2. Filter location data
# Filtering is based on selected parameters from above
source("analysis/tracking/scr/filter_locs.R")

# Step 3. Regularize location data
# Uses correlated random walk state-space model from Jonsen et al. 2019 doi:10.1002/ecy.2566
source("analysis/tracking/scr/regularize_ssm.R")