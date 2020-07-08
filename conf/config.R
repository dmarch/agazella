#-----------------------------------------------------------------------------------------
# config.R          Configure data paths and general parameters
#-----------------------------------------------------------------------------------------


# Set local path to the data repository
# data_dir <- "D:/Data/movemed-multispecies"

# Load common libraries
library(stringr)
library(dplyr)
library(lubridate)
library(grid)
library(gridExtra)
library(rgdal)
library(raster)
library(foreach)  
library(doParallel)
library(reshape2)
library(ggplot2)
library(data.table)
library(pals)
library(ncdf4)

# Set project projections
crs_proj <- "+init=epsg:4326"
