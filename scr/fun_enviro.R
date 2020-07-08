#-----------------------------------------------------------------------------------------
# fun_enviro.R            Suite of common functions for handling environmental data
#-----------------------------------------------------------------------------------------
# BigCmems2Daily          Derive daily from large nc
# download_cmems_daily      Custom fuction to download and store CMEMS daily files
# extract_dynamic         Extract data from dynamic variables
# import_catalog            Import environmental data cataloge
# week_list                 Generate a sequence of week days





#------------------------------------------------------------------
# BigCmems2Daily          Derive daily from large nc
#------------------------------------------------------------------
BigCmems2Daily <- function(ncfile, var, catalog, cores, daily.mean = TRUE){
  
  # Prepare cluster
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  
  # locate product by variable name and date
  c <- dplyr::filter(catalog, movemed_var == var)
  cmems_repo <- c$root
  output_service <- c$service
  output_product <- c$product
  output_var <- c$variable
  output_name <- as.character(c$standard_name)
  
  # import data
  b <- brick(ncfile)
  
  # extract temporal information from layer names
  rdates <- names(b)
  Y <- substr(rdates, start=2, stop=5)
  m <- substr(rdates, start=7, stop=8)
  d <-substr(rdates, start=10, stop=11)
  H <- substr(rdates, start=13, stop=14)
  M <- substr(rdates, start=16, stop=17)
  S <-substr(rdates, start=19, stop=20)
  
  # set dates
  times <- paste(paste(Y,m,d, sep="-"), paste(H,M,S, sep=":"))
  times <- parse_date_time(times, "Ymd HMS", tz="UTC")
  dates <- as.Date(times)
  udates <- unique(dates)
  
  # calculate daily mean
  foreach(i=1:length(udates), .packages=c("lubridate", "raster", "stringr", "dplyr")) %dopar% {
    
    # subset layer from given day
    idate <- udates[i]
    idx <- which(dates == idate)
    subr <- subset(b, idx)
    
    # calculate daily mean
    if (daily.mean == TRUE){
      p <- mean(subr, na.rm=TRUE)
    } else {
      p <- subr
    }
    
    
    # export raster into product folder
    YYYY <- year(idate)
    MM <- sprintf("%02d", month(idate))
    DD <- sprintf("%02d", day(idate))
    
    ## Export output raster
    product_folder <- paste(cmems_repo, output_service, output_product, YYYY, MM, sep="/")  # Set folder
    if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
    file_name <- paste0(YYYY, MM, DD, "_", output_product, "_", output_var, ".nc")  # Set file name
    file_path <- paste(product_folder, file_name, sep="/")  # Set file path 
    writeRaster(p, filename = file_path, format="CDF", overwrite=TRUE,
                varname = output_var, longname = output_name, xname="lon", yname="lat") 
  }
  
  
  # Stop cluster
  stopCluster(cl)
}
#------------------------------------------------------------------



#------------------------------------------------------------------------------------
# download_cmems_daily_REMOVE      Custom fuction to download and store CMEMS daily files
#------------------------------------------------------------------------------------
download_cmems_daily_REMOVE <- function(date, cmems_repo, cfg, ROI, n_tries = 10, sleep = 1){
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


#------------------------------------------------------------------------------------
# download_cmems_daily     Custom fuction to download and store CMEMS daily files
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
  
  # Set min max hours
  time_min <- as.POSIXct(date, tz="UTC")
  time_max <- time_min + 86399
  
  # Format times
  time_min <- as.character(format(time_min, "%Y-%m-%d %H:%M:%S"))
  time_max <- as.character(format(time_max, "%Y-%m-%d %H:%M:%S"))
  
  # Download data and generate file
  try_number <- 1
  while(!file.exists(file_path) && try_number <= n_tries){
    try_number = try_number + 1
    
    result <- CMEMS.download.advanced(cfg, out.dir = product_folder, out.name = file_name,
                                      date.min = time_min, date.max = time_max, latitude.min = ROI@ymin,
                                      latitude.max = ROI@ymax, longitude.min = ROI@xmin, longitude.max = ROI@xmax,
                                      depth.min = NULL, depth.max = NULL, debug = FALSE)
    
    Sys.sleep(sleep)
  }
}
#------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------
# extract_dynamic         Extract data from dynamic variables
#---------------------------------------------------------------------------------
extract_dynamic <- function(var, date, lon, lat, buffer, catalog){
  # use the catalog as main source to locate where the data is found
  
  
  library(stringr)
  library(lubridate)
  
  # locate product by variable name and date
  c <- dplyr::filter(catalog, movemed_var == var, as.Date(date) >= date_min & as.Date(date) <= date_max)
  
  # get date information
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # get frequency of the product and set threshold
  # A threshold is define in order to extract data from the closest day/week in case no file available
  # for a given date
  freq <- c$temporal_resolution
  if(freq == "daily") temp_thrs <- 1  # will get data from previous/next day
  if(freq == "weekly") temp_thrs <- 7  # will get data from previous/next week
  if(freq == "monthly")  temp_thrs <- 30  # will get data from previous/next month
  
  # locate folder
  repo = c$root
  service = c$service
  product = c$product
  product_folder <- paste(repo, service, product, YYYY, MM, sep="/")  # Set folder
  
  # list dates of files
  variable = c$variable
  folder_files <- list.files(product_folder, full.names=TRUE, patter = variable)  # import to differentiate between vars
  folder_dates <- ymd(as.numeric(str_extract(folder_files, "\\d{8}")))
  
  # find closest date within frequency threshold
  dif_time <- as.numeric(abs(difftime(as.Date(date), folder_dates, units = "days")))
  
  # check that there is data to retrieve
  if (any(dif_time <= temp_thrs)){
    
    # select file
    sel_dif <- which(dif_time == min(dif_time))[1]  # in case >1 selected (eg. no data for date i, and select both previous and after)
    file_path <- folder_files[sel_dif]
    
    # import file
    r <- raster(file_path, var=variable)  # open nc
    
    # extract information
    ncdata <- raster::extract(r, cbind(lon, lat), buffer=buffer, fun=mean, na.rm=TRUE)  # extract data
  } else {
    ncdata <- NA
  }
  #return extracted data
  return(list(ncdata))
}
#---------------------------------------------------------------------------------


#---------------------------------------------------------------------------------
# extract_stack         Extract data from environmental stack
#---------------------------------------------------------------------------------
extract_stack <- function(date, lon, lat, buffer, repo = "data/out/environment/stack_daily"){
  # use the catalog as main source to locate where the data is found
  
  library(stringr)
  library(lubridate)
  
  # get date information
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # locate folder
  product_folder <- paste(repo, YYYY, MM, sep="/")  # Set folder
  
  # Locate file
  pat <- paste0(format(date, "%Y%m%d"), "_enviro.grd")
  grdfile <- list.files(repo, recursive = TRUE, full.names = TRUE, pattern = pat)
  
  # Import environmental stack
  s <- stack(grdfile)
  
  # Check if points overlaps stack extent
  if((lon >= s@extent@xmin & lon <= s@extent@xmax) &
     (lat >= s@extent@ymin & lat <= s@extent@ymax)){
    
    # if overlap, extract information
    ncdata <- raster::extract(s, cbind(lon, lat), buffer=buffer, fun=mean, na.rm=TRUE)  # extract data
    
    # reduce amount of decimals
    decs <- nchar(sub(".*\\.(0*).*","\\1", ncdata))+2
    ncdata <- round(ncdata, decs)
    
  } else {
    
    # if no overlap, create empty matrix
    ncdata <- matrix(data = NA, nrow = 1, ncol = nlayers(s), dimnames = list(NULL, names(s)))
  }

  #return extracted data
  return(data.frame(ncdata))
}
#---------------------------------------------------------------------------------


#---------------------------------------------------------------------------------
# import_catalog        Import environmental data cataloge
#---------------------------------------------------------------------------------
import_catalog <- function(file, provider_paths=NULL){
  # file: csv file with catalogue of products
  # provider_paths: list. paths to repositories
  #
  # file <- "environment/catalog.csv"
  # provider_paths <- list(CMEMS = paste0(data_dir, "/input/environment/cmems"),
  #                        MOVEMED = paste0(data_dir, "/input/environment/cmems"),
  #                        AVISO = paste0(data_dir, "/input/environment/aviso"))
  # catalog <- import_catalog(file, provider_paths)
  
  # import product catalog
  catalog <- read.csv(file)
  
  # set data formats
  catalog$motu_server <- as.character(catalog$motu_server)
  catalog$service <- as.character(catalog$service)
  catalog$product <- as.character(catalog$product)
  catalog$variable <- as.character(catalog$variable)
  catalog$depth_min <- as.character(catalog$depth_min)
  catalog$depth_max <- as.character(catalog$depth_max)
  catalog$date_min <- dmy(catalog$date_min)
  catalog$date_max <- dmy(catalog$date_max)
  
  # Set root paths for each repository
  if(!is.null(provider_paths)){
    catalog$root <- NA
    catalog$root[catalog$provider == "CMEMS"] <- provider_paths$CMEMS
    catalog$root[catalog$provider == "MOVEMED"] <- provider_paths$MOVEMED
    catalog$root[catalog$provider == "AVISO"] <- provider_paths$AVISO
  }

  return(catalog)
  
}
#---------------------------------------------------------------------------------


#---------------------------------------------------------------------------------
# prepareGrid        Prepare raster to add into stack
#---------------------------------------------------------------------------------
prepareGrid <- function(r, m, method, name){
  rc <- crop(r, m)  # crop by extent
  rs <- resample(r, m, method=method)  # resample
  rm <- mask(rs, m)  # mask
  names(rm) <- name
  return(rm)
}
#---------------------------------------------------------------------------------


#---------------------------------------------------------------------------------
# prepareGridCatalogue         Extract data from dynamic variables
#---------------------------------------------------------------------------------
prepareGridCatalogue <- function(var, date, catalog, m, method, name){
  # use the catalog as main source to locate where the data is found
  
  
  library(stringr)
  library(lubridate)
  
  # create empty map with NA in case no file is found
  empty_m <- m
  empty_m[empty_m == 1] <- NA
  
  # locate product by variable name and date
  c <- dplyr::filter(catalog, movemed_var == var, as.Date(date) >= date_min & as.Date(date) <= date_max)
  
  # get date information
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # get frequency of the product and set threshold
  # A threshold is define in order to extract data from the closest day/week in case no file available
  # for a given date
  freq <- c$temporal_resolution
  if(freq == "daily") temp_thrs <- 1  # will get data from previous/next day
  if(freq == "weekly") temp_thrs <- 8  # will get data from previous/next week
  if(freq == "monthly")  temp_thrs <- 30  # will get data from previous/next month
  
  # locate folder
  repo = c$root
  service = c$service
  product = c$product
  product_folder <- paste(repo, service, product, YYYY, MM, sep="/")  # Set folder
  
  # list dates of files
  variable = c$variable
  folder_files <- list.files(product_folder, full.names=TRUE, patter = variable)  # import to differentiate between vars
  folder_dates <- ymd(as.numeric(str_extract(folder_files, "\\d{8}")))
  
  
  # find closest date within frequency threshold
  dif_time <- as.numeric(abs(difftime(as.Date(date), folder_dates, units = "days")))
  
  # check that there is data to retrieve
  if (any(dif_time <= temp_thrs)){
    
    # select file
    sel_dif <- which(dif_time == min(dif_time))[1]  # in case >1 selected (eg. no data for date i, and select both previous and after)
    file_path <- folder_files[sel_dif]
    
    # import file
    r <- raster(file_path, var=variable)  # open nc
    
    # resample at common grid
    pg <- prepareGrid(r, m, method=method, name=name)
    
    
  } else {
    pg <- empty_m
    names(pg) <- name
  }
  
  return(pg)
}
#---------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------
# week_list       Generate a sequence of week days
#------------------------------------------------------------------------------------------
week_list <- function(start_date, end_date, by = "8 days"){
  # CMEMS weekly products are based on 8 day intervals. Every new year, they restart.
  # Therefore, it is useful to have a list of days that match their criteria.
  # This function returns a list of days that match with such criteria given a start and end date
  #
  # Arguments
  # start_date        Date format
  # end_date          Date format
  # by                increment of the sequence. See ?seq.Date
  
  # load libraries
  library(lubridate)
  library(dplyr)
  library(data.table)
  
  # get first and last year
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  # get sequence of years
  seq_year <- seq(start_year, end_year, by = 1)
  
  # for each year, create weekly sequence
  wk_list <- list()
  for(i in 1:length(seq_year)){
    jan1 <- as.Date(paste0(seq_year[i], "-01-01"))
    dec31 <- as.Date(paste0(seq_year[i], "-12-31"))
    wk_seq <- seq(jan1, dec31 , by)
    wk_list[[i]] <- list(weekday = wk_seq)
  }
  
  # combine week sequences
  weeks <- rbindlist(wk_list)
  
  # filter out days that are not within start and end dates
  weeks <- dplyr::filter(weeks, weekday >= start_date, weekday <= end_date)
  
  # returb vector of dates
  return(weeks$weekday)
}
#------------------------------------------------------------------------------------------
