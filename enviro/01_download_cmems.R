#-------------------------------------------------------------------------
# cmems_subsetter.R
#-------------------------------------------------------------------------

# Load dependencies
# devtools::install_github("markpayneatwork/RCMEMS")
library(RCMEMS)
library(raster)
library(lubridate)
library(foreach)  
library(doParallel)


# Note
# potential problem when running the script was fixed by updating the MOTU version.


#------------------------------------------------------------------------------------
# download_cmems_daily      Custom fuction to download and store CMEMS daily files
#------------------------------------------------------------------------------------
download_cmems_daily <- function(date, cmems_repo, cfg, ROI, n_tries = 10, sleep = 1){
  # Folder name: provider/service/product/YYYY/MM/
  # File name: YYYMMDD_product_variable.nc
  
  
  # set day information
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Create folder repository
  product_folder <- paste(cmems_repo, cfg@service.id, cfg@product.id, YYYY, MM, sep="/")
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # Output filename
  file_name <- paste0(YYYY, MM, DD, "_", cfg@product.id, "_", cfg@variable, ".nc")
  file_path <- paste(product_folder, file_name, sep="/")
  
  # Download data and generate file
  try_number <- 1
  while(!file.exists(file_path) && try_number <= n_tries){
    try_number = try_number + 1
    result <- CMEMS.download(cfg,
                             ROI = ROI,
                             date.range = c(date, date),
                             out.path = file_path,
                             debug = FALSE)
    Sys.sleep(sleep)
  }
}
#------------------------------------------------------------------------------------




# set repository for CMEMS products
cmems_repo <- paste0(input_data, "/cmems") #"D:/Data/agazella/cmems"


# import product catalog
catalog <- read.csv("cmems/agazella_catalog.csv")  # list with updated products for 2019

# set data formats
catalog$motu_server <- as.character(catalog$motu_server)
catalog$service <- as.character(catalog$service)
catalog$product <- as.character(catalog$product)
catalog$variable <- as.character(catalog$variable)
catalog$depth_min <- as.character(catalog$depth_min)
catalog$depth_max <- as.character(catalog$depth_max)
catalog$date_min <- dmy(catalog$date_min)
catalog$date_max <- dmy(catalog$date_max)

# read user and password from file
keyfile = "auth/cmems_keys.json"
keys <- jsonlite::fromJSON(keyfile)

# define bounding box to subset
e <- extent(-90, -20, -80, -40)

# select datasets
catalog <- dplyr::filter(catalog, provider == "CMEMS")

## Prepare clusters
cores <- 10  # detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


for (i in 1:nrow(catalog)){

  print(paste("Processing", catalog$standard_name[i], "from", catalog$product[i]))
  
  # define config file
  if(is.na(catalog$depth_min[i])){
    cfg <- CMEMS.config(python="python",
                        script="C:/motuclient-python/motuclient.py",
                        motu=catalog$motu_server[i],
                        service.id = catalog$service[i],
                        product.id = catalog$product[i],
                        variable = catalog$variable[i],
                        user = keys$user,
                        pwd = keys$pwd)
  } else {
    cfg <- CMEMS.config(python="python",
                        script="C:/motuclient-python/motuclient.py",
                        motu=catalog$motu_server[i],
                        service.id = catalog$service[i],
                        product.id = catalog$product[i],
                        variable = catalog$variable[i],
                        depth.min = catalog$depth_min[i],
                        depth.max = catalog$depth_max[i],
                        user = keys$user,
                        pwd = keys$pwd)
  }

  
  # define daily OR weekly sequence
  if(catalog$temporal_resolution[i] == "weekly") {
    dates <- week_list(start_date = catalog$date_min[i], end_date = catalog$date_max[i], by = "8 days")
  }
  
  if(catalog$temporal_resolution[i] == "daily"){
    dates <- seq(from = catalog$date_min[i], to = catalog$date_max[i], by = "day")
  }

  # loop for each day
  # It take approach 8 min per variable per year
  y <- foreach(j=1:length(dates), .packages=c("lubridate", "RCMEMS"), .inorder=TRUE) %dopar% {

    Sys.sleep(sample(3:10, 1)) # 1:5 works well
    download_cmems_daily(date = dates[j], cmems_repo = cmems_repo, cfg = cfg, ROI = e, n_tries = 10, sleep = 10)  # sleep 3
  }
}

# stop cluster
stopCluster(cl)