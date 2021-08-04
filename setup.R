#--------------------------------------------------------------------------------
# setup.R         Setup project
#--------------------------------------------------------------------------------

# set computer
cpu <- "server"  # "pc", "mac". "server"

# If you want to give MaxEnt (the Java virtual machine that runs it) more memory,
# you can do that by running something like this (for 1 GB) before you load the dismo library.
options(java.parameters = "-Xmx4g" )

# Load required packages
pacman::p_load("data.table", "tidyr", "dplyr", "lubridate", "readxl", "stringr", "reshape2", "splitstackshape", # data manipulation
               "ggplot2", "egg", "pals", "viridis", "gridExtra", "grid", "scales", "ggprism", "ggimage", "RColorBrewer", # plots
               "foreach", "doParallel",  # parallel computing
               "move", "moveVis", "SDLfilter", "adehabitatHR", "foieGras", "argosfilter",  # movement
               "availability", # https://github.com/AustralianAntarcticDataCentre/availability
               "corrplot", "dismo", "gbm", "randomForest", "Hmisc", "ggBRT", "groupdata2", # habitat model
               "rJava", "fmsb",  # miscellaneous
               "rnaturalearthdata", "rnaturalearth",  # spatial data
               "raster", "sf", "ncdf4", "rgeos","maptools", "gdistance",  # spatial
               "rdrop2",
               install = FALSE)  # dropbox data

# Set project projections
crs_proj <- "+init=epsg:4326"


# Set main data paths
if(cpu == "pc") main_dir <- "D:/Dropbox/Dropbox/GitData/agazella"
if(cpu == "mac") main_dir <- "~/Dropbox/GitData/agazella"
if(cpu == "server") main_dir <- "data"

# 2. Create data paths
input_data <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_data)) dir.create(input_data, recursive = TRUE)

output_data <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

