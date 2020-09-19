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

#-----------------------------------------------
# Import data catalog
#-----------------------------------------------

# set repository for CMEMS products
cmems_repo <- "D:/Data/agazella/cmems"

# import product catalog
catalog <- read.csv("cmems/agazella_catalog.csv")  # list with updated products for 2019
catalog$date_min <- dmy(catalog$date_min)
catalog$date_max <- dmy(catalog$date_max)

# Set roots for each repository
catalog$root <- NA
catalog$root[catalog$provider == "CMEMS"] <- "D:/Data/agazella/cmems"
catalog$root[catalog$provider == "MOVEMED"] <- "D:/Data/agazella/cmems"

#-----------------------------------------------
# Set initial parameters: dates for analysis
#-----------------------------------------------

# generate daily sequence
dayseq <- seq(as.Date("2019-02-01"), as.Date("2019-09-30"), by = "day")

# import map of distance to shore
sdist <- raster("D:/Data/agazella/gebco/derived_sdist.nc")


#-----------------------------------------------
# Retrieve raster
#-----------------------------------------------

# import ice concentration
date <- dayseq[90]
sic <- extract_raster(varname = "SIC", date = date, catalog = catalog)

# transform to extent based on % threshold
sic[sic < 0.15] <- NA
sic[sic >= 0.15] <- 1

# resample to coarser resolution
sic_coarse <- resample(sic, sdist, method="ngb")


# calculate distance to ice extent
idist <- distance(sic_coarse )  # calculate distance

m <- mask(idist, sdist)
m <- mask(m, sic_coarse, inverse = TRUE)

#-----------------------------------------------
# Process data
#-----------------------------------------------


## Prepare clusters
cores <- 18  # detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

y <- foreach(i=1:length(dayseq), .packages=c("raster", "lubridate"), .inorder=FALSE) %dopar% {
  
  ## Get date
  date <- dayseq[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  ## Import file
  product_folder <- paste(cmems_repo, input_service, input_product, YYYY, MM, sep="/")  # Set folder
  file_name <- paste0(YYYY, MM, DD, "_", input_product, "_", input_var, ".nc")  # Set file name
  file_path <- paste(product_folder, file_name, sep="/")  # Set file path 
  r <- raster(file_path, var=input_var)  # import file
  
  ## Generate derived product
  p <- terrain(r, opt="slope", unit="degrees", neighbors = 4)
  setZ(p, getZ(r))
  names(p) <- output_var

  
  ## Export output raster
  product_folder <- paste(cmems_repo, output_service, output_product, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  file_name <- paste0(YYYY, MM, DD, "_", output_product, "_", output_var, ".nc")  # Set file name
  file_path <- paste(product_folder, file_name, sep="/")  # Set file path 
  writeRaster(p, filename = file_path, format="CDF", overwrite=TRUE,
              varname = output_var, longname = output_name, xname="lon", yname="lat") 

}

# stop cluster
stopCluster(cl)
