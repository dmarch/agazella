#------------------------------------------------------------------------------------------
# fun_data_processing.R         Suite of functions to process data
#------------------------------------------------------------------------------------------




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

    # select columns
    df <- select(df, ptt = Tag_ID, date = date, lon = Longitude, lat = Latitude, lc = Location.Quality, habitat = `Habitat(1=land;2=water;.3=.ice)`)
    
    # extract ptt number
    df$ptt <- as.numeric(sub('.*\\:', '', df$ptt))
    
    # remove row without location data
    df <- dplyr::filter(df, !is.na(lon))
    
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
