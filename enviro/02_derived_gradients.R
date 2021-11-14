#-------------------------------------------
# derived_gradients.R
#-------------------------------------------
# Date: 2017/11/23
# Author: David March
# Project: Tortugas oceanografas
#
# Description:
# Generate repo for gradients of SST


# Load libraries
library(raster)
library(foreach)  
library(doParallel)
library(lubridate)

#-----------------------------------------------
# Import data catalog
#-----------------------------------------------

# set repository for CMEMS products
cmems_repo <- paste0(input_data, "/cmems")

# import product catalog
catalog <- read.csv("cmems/agazella_catalog.csv")  # list with updated products for 2019

# set data formats
catalog$service <- as.character(catalog$service)
catalog$product <- as.character(catalog$product)
catalog$variable <- as.character(catalog$variable)
catalog$standard_name <- as.character(catalog$standard_name)
catalog$date_min <- dmy(catalog$date_min)
catalog$date_max <- dmy(catalog$date_max)

#-----------------------------------------------
# Set initial parameters: gradient
#-----------------------------------------------

input_product_id <- 2
output_product_id <- 11


#-----------------------------------------------
# Retrieve information
#-----------------------------------------------

# input data
input_service <- catalog$service[input_product_id]
input_product <- catalog$product[input_product_id]
input_var <- catalog$variable[input_product_id]

# output data
output_service <- catalog$service[output_product_id]
output_product <- catalog$product[output_product_id]
output_var <- catalog$variable[output_product_id]
output_name <- catalog$standard_name[output_product_id]

# set dates
start_date <- catalog$date_min[input_product_id]
end_date <- catalog$date_max[input_product_id]

# generate daily sequence
dayseq <- seq(as.Date(start_date), as.Date(end_date), by = "day")


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
