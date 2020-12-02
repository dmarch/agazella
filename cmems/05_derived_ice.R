#-------------------------------------------
# derived_ice.R
#-------------------------------------------
# Date: 2017/11/23
# Author: David March
#
# Description:
# Generate repo for ice extent


# Load libraries
library(raster)
library(foreach)  
library(doParallel)
library(lubridate)

source("../caldio/scr/extract_tools.R")
source("scr/fun/fun_data_processing.R")

#------------------------------------------------------------------------------
# distToIceEdge
#------------------------------------------------------------------------------
distToIceEdge <- function(sic, oceanmask, thrs=0.15){
  # Calculate distance to ice edge
  # x: ice concentration raster
  # thrs: concentration threshold used to define the edge
  # oceanmask: raster with NA values on land cells
  #
  # returns raster with distance to edge.
  # positive values from ocean, negative from ice
  
  # transform to extent based on % threshold
  sic[sic >= thrs] <- 1  # sea icea (1)
  sic[sic < thrs] <- 2  # ocean (2)
  sic[is.na(sic)] <- 2
  
  # resample to coarser resolution
  sic_coarse <- resample(sic, oceanmask, method="ngb")
  
  # calculate distance to ice extent (from the ocean, positive)
  idist <- gridDistance(sic_coarse, origin = 1) 
  idist <- mask(idist, oceanmask)
  
  # calculate distance to ice extent (from the ice, negative)
  idist2 <- gridDistance(sic_coarse, origin = 2) 
  idist2 <- mask(idist2, oceanmask)
  idist2 <- idist2 * (-1)
  
  # combine
  dist <- sum(idist, idist2)/1000 # transform to km
  return(dist)
}
#------------------------------------------------------------------------------




#-----------------------------------------------
# Import data catalog
#-----------------------------------------------

# set repository for CMEMS products
cmems_repo <- "data/cmems"

# import product catalog
catalog <- read.csv("cmems/agazella_catalog.csv")  # list with updated products for 2019
catalog$date_min <- dmy(catalog$date_min)
catalog$date_max <- dmy(catalog$date_max)

# Set roots for each repository
catalog$root <- NA
catalog$root[catalog$provider == "CMEMS"] <- "data/cmems"
catalog$root[catalog$provider == "MOVEMED"] <- "data/cmems"

#-----------------------------------------------
# Set initial parameters: dates for analysis
#-----------------------------------------------

# generate daily sequence
dayseq <- seq(as.Date("2019-02-01"), as.Date("2019-09-30"), by = "day")

# import map of distance to shore
sdist <- raster("data/gebco/derived_sdist.nc")


#-----------------------------------------------
# Process distance to ice edge
#-----------------------------------------------

## Prepare clusters
cores <- 10  # detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


y <- foreach(i=1:length(dayseq), .packages=c("raster", "lubridate"), .inorder=FALSE) %dopar% {

  # import ice concentration
  date <- dayseq[i]
  sic <- extract_raster(varname = "SIC", date = date, catalog = catalog)
  
  # calculate distance to ice edge
  edgedist <- distToIceEdge(sic, oceanmask = sdist, thrs = 0.15)
  
  # export raster
  export_raster(r=edgedist, varname="EDGE", date=date, catalog=catalog)
}

# stop cluster
stopCluster(cl)

