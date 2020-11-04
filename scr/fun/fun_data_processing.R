#------------------------------------------------------------------------------------------
# fun_data_processing.R         Suite of functions to process data
#------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------
# extract_raster         Extract data from dynamic variables
#---------------------------------------------------------------------------------
extract_raster <- function(varname, date, catalog){
  # use the catalog as main source to locate where the data is found
  
  
  library(stringr)
  library(lubridate)
  
  # locate product by variable name and date
  c <- dplyr::filter(catalog, movemed_var == varname, as.Date(date) >= date_min & as.Date(date) <= date_max)
  
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
  variable = as.character(c$variable)
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
    
  } else {
    r <- NULL
  }
  #return extracted data
  return(r)
}
#---------------------------------------------------------------------------------






#---------------------------------------------------------------------
# L02L1_sdl     Function to convert L0 to L1 location using SDLfilter
#---------------------------------------------------------------------
L02L1_sdl <- function (data, vmax = 40, step.time = 5/60, step.dist = 0.001){
  
  require(SDLfilter)
  
  
  ## Keep original number of locations
  loc0 <- nrow(data)
  
  ## Convert standard format to SDLfilter
  
  # Standardize Location clasess
  data$lc <- as.character(data$lc)
  data$lc[data$lc == "A"] <- -1 
  data$lc[data$lc == "B"] <- -2
  data$lc[data$lc == "Z"] <- -9
  data$lc <- as.numeric(data$lc)
  
  # Rename columns
  names(data)[names(data)=="ptt"] <- "id"
  names(data)[names(data)=="date"] <- "DateTime"
  names(data)[names(data)=="lc"] <- "qi"
  
  # ### Filter point on land
  # data$onland <- point.on.land(lat = data$lat, lon = data$lon)
  # data <- filter(data, onland == FALSE)
  
  ### Remove duplicated locations, based on both time and space criteria
  data <- dupfilter(data, step.time=step.time, step.dist=step.dist, conditional = FALSE)
  
  ## Filter out Z location classess
  data <- filter(data, qi > -9)
  
  ## Filter out values above speed threshold, considering EITHER previous and subsequent positions
  data <- ddfilter.speed(data, vmax = vmax, method = 1)
  
  ## If speed from first or last location are above the threshold, then remove them.
  if (data$sSpeed[1] > vmax) data <- data[-1,]
  if (data$pSpeed[nrow(data)] > vmax) data <- data[-nrow(data),]
  
  ## Estimate vmax from data
  V <- est.vmax(data, qi = 1)
  
  ## Back transform data.frame to standar format
  
  # Rename columns
  names(data)[names(data)=="id"] <- "ptt"
  names(data)[names(data)=="DateTime"] <- "date"
  names(data)[names(data)=="qi"] <- "lc"
  
  # Standardize Location clasess
  data$lc[data$lc == -1] <- "A" 
  data$lc[data$lc == -2] <- "B"
  
  ## Create a data.frame with processing report data
  proc <- data.frame(ptt = data$ptt[1], locL0 = loc0, locL1 = nrow(data),
                     percent_filtered = round(((loc0-nrow(data))/loc0)*100, 2), vmax_kmh = V)
  
  
  ## Prepare output
  out <- list(data = data, proc = proc)
  return(out)
}
#---------------------------------------------------------------------



#----------------------------------------------------------------------
# read_agazella       Read processed data from Cardona
#----------------------------------------------------------------------
read_agazella <- function(file){
  # Sitrack data is formatted in a Excel file by Lluis Cardona
  # Data for each tagged animal is stored in a individual sheet
  # We will get habitat classification AND deployment date from this data

  # Load libraries
  library(dplyr)
  library(openxlsx)
  library(data.table)
  
  # custom function embeded
  xls2degrees <- function(x){
    # Manipulating data in Excel can generate problems in coordinates because of changes of the
    # regional system.
    # This function fixes this issue. But assumes that coordinates are XX.XXXX
    
    library(stringr)
    
    
    lon_length <- str_length(x)  # get the length of the string
    lon_num <- as.numeric(x)  # convert the string to a number
    dec <- str_detect(x, pattern = "\\.", negate = FALSE)  # identify if there is a decimal separator
    minus <- str_detect(x, pattern = "-", negate = FALSE)  # identify if there is a negative coordinate
    decimals <- ifelse(minus == "TRUE", lon_length-3, lon_length-2)  # define number of decimals
    lon <- ifelse(dec == "TRUE", lon_num, lon_num/10^(decimals))
    
    return(lon)
  }
  
  # 1. Get the list of all sheets 
  sheet_names <- getSheetNames(file)
  

  # 2. Loop to process each sheet and combine with the others
  data_list <- list()
  
  for (i in 1:length(sheet_names)){
    
    # import data
    df <- read.xlsx(xlsxFile = file, sheet = i, detectDates = FALSE)
 
    # process date time
    options(digits=20)
    df$date <- df$UTC_Date + df$UTC_Time
    df$date <- as.POSIXct(df$date*3600*24, tz="GMT", origin = "1900-01-01 00:00:00") -(2*3600*24)
    options(digits=7)  # return to default value
    
    
    # select columns
    df <- dplyr::select(df, ptt = Tag_ID, date = date, lon = Longitude, lat = Latitude, lc = Location.Quality, habitat = `Habitat(1=land;2=water;.3=.ice)`)
    
    # extract ptt number
    df$ptt <- as.numeric(sub('.*\\:', '', df$ptt))
    
    # remove row without location data
    df <- dplyr::filter(df, !is.na(lon))
    
    # fix coordinates problems
    df$lon <- xls2degrees(df$lon)
    df$lat <- xls2degrees(df$lat)

    # append to data_list
    data_list[[i]] <- df
  }

  # combine all tags
  data <- rbindlist(data_list)
  
  # filter out locations without habitat
  data <- dplyr::filter(data, !is.na(habitat))

  return(data)
}


#----------------------------------------------------------------------
# read_sirtrack       Read raw data from Sirtrack
#----------------------------------------------------------------------
read_sirtrack <- function(file){

  # load libraries
  library(dplyr)
  library(stringr)
  library(lubridate)
  
  # import file
  df <- read.csv(file)
  
  # create date
  df$date <- paste(df$UTC_Date, df$UTC_Time)
  df$date <- parse_date_time(df$date, "Ymd HMS")
  
  # select columns
  df <- select(df, ptt = Tag_ID, date = date, lon = Longitude, lat = Latitude, lc = Location.Quality)
  
  # extract ptt number
  df$ptt <- as.numeric(sub('.*\\:', '', df$ptt))
  
  # remove row without location data
  df <- dplyr::filter(df, !is.na(lon))
  return(df)
}



#--------------------------------------------------------------------------------------
# readTrack       reads standardized animal track data in csv
#--------------------------------------------------------------------------------------
readTrack <- function(csvfiles){
  # Description
  # Reads a standardized animal track data in csv.
  # Returns a data frame with parsed time
  # It allows the combination of multiple files
  # csvfiles: string with the location of 1 or more csv files
  
  library(lubridate)
  library(data.table)
  
  ## create empty list
  dt_list <- list()  
  
  ## process and append files
  for (i in 1:length(csvfiles)){
    data <- read.csv(csvfiles[i], header=TRUE)  # read csv
    data$date <- parse_date_time(data$date, "Ymd HMS") # parse time
    dt_list[[i]] <- data  # append to list
  }
  
  dt <- rbindlist(dt_list)  # combine data.frames
  return(dt)
}
#--------------------------------------------------------------------------------------
