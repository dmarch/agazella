#------------------------------------------------------------------------------------
# reading_tools.R    Suite of function for reading and transforming data
#------------------------------------------------------------------------------------
# This script contains the following custom functions:
#
# read_agazella       Read processed data from Cardona
# readTrack           Read standardized animal track data in csv
#------------------------------------------------------------------------------------


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
    df <- dplyr::select(df, id = Tag_ID, date = date, lon = Longitude, lat = Latitude, lc = Location.Quality, habitat = `Habitat(1=land;2=water;.3=.ice)`)
    
    # extract ptt number
    df$id <- as.numeric(sub('.*\\:', '', df$id))
    
    # remove row without location data
    df <- dplyr::filter(df, !is.na(lon))
    
    # fix coordinates problems
    df$lon <- xls2degrees(df$lon)
    df$lat <- xls2degrees(df$lat)
    
    # Create KF error ellipse information
    # Current version contains empty parameters
    df$smaj <- NA
    df$smin <- NA
    df$eor <- NA
    
    # append to data_list
    data_list[[i]] <- df
  }
  
  # combine all tags
  data <- rbindlist(data_list)
  
  # filter out locations without habitat
  data <- dplyr::filter(data, !is.na(habitat))
  
  return(data)
}



#---------------------------------------------------------------------
# readTrack     Read standardized animal track data in csv
#---------------------------------------------------------------------
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
  
  dt <- rbindlist(dt_list, fill=TRUE)  # combine data.frames
  return(dt)
}
#---------------------------------------------------------------------


