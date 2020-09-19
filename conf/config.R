#-----------------------------------------------------------------------------------------
# config.R          Configure data paths and general parameters
#-----------------------------------------------------------------------------------------


# Set local path to the data repository
# data_dir <- "D:/Data/movemed-multispecies"

# Load common libraries
library(SDLfilter)
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
library(argosfilter)
library(foieGras)
library(openxlsx)
library(rworldxtra)


# Set project projections
crs_proj <- "+init=epsg:4326"
