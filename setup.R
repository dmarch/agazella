#--------------------------------------------------------------------------------
# setup.R         Setup project
#--------------------------------------------------------------------------------

# set computer
cpu <- "server"  # "pc", "mac". "server"

# If you want to give MaxEnt (the Java virtual machine that runs it) more memory,
# you can do that by running something like this (for 1 GB) before you load the dismo library.
options(java.parameters = "-Xmx4g" )

# Load required packages
pacman::p_load("data.table", "tidyr", "dplyr", "lubridate", "openxlsx", "stringr", "reshape2", # data manipulation
               "ggplot2", "egg", "pals", "viridis", "gridExtra", "grid", "scales",  # plots
               "foreach", "doParallel",  # parallel computing
               "move", "moveVis", "SDLfilter", "adehabitatHR", "foieGras", "argosfilter",  # movement
               "availability", # https://github.com/AustralianAntarcticDataCentre/availability
               "corrplot", "dismo", "gbm", "randomForest", "Hmisc", # habitat model
               "rJava", "fmsb",  # miscellaneous
               "rnaturalearthdata", "rnaturalearth",  # spatial data
               "raster", "sf", "ncdf4", "rgeos")  # spatial

# Set project projections
crs_proj <- "+init=epsg:4326"


# Set main data paths
if(cpu == "pc") main_dir <- "D:/Dropbox/Dropbox/GitData/agazella"
if(cpu == "mac") main_dir <- "~/Dropbox/GitData/agazella"
if(cpu == "server") main_dir <- "data"

# 2. Create data paths
# The project involves creating three main folders into the working directory to manage the data:
#ifelse(!dir.exists("data"), dir.create("data"), "Folder 'data' exists already")  # contains raw data
#ifelse(!dir.exists("temp"), dir.create("temp"), "Folder 'temp' exists already")  # contains temporary files (required for further steps)
#ifelse(!dir.exists("out"), dir.create("out"), "Folder 'out' exists already")  # contains output files
#ifelse(!dir.exists("out/proc"), dir.create("out/proc"), "Folder 'proc' exists already")  # contains output files
input_data <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_data)) dir.create(input_data, recursive = TRUE)

output_data <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


# 3. Store raw data into /data folder

# Trackin data
# data/TelemetriaPPT.xlsx