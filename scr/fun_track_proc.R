#------------------------------------------------------------------------------------
# fun_track_proc.R    Suite of function for procssing location data
#------------------------------------------------------------------------------------
# This script contains the following custom functions:
#
# embc_vars         Estimate EMbC movement variables
# filter_track      Function to convert L0 to L1 location using SDLfilter
# landMask          Funcion to check if a track falls into the oceanMask
# segmentOnPort     Segment trips based on overlap with ports
# summarizeId       Sumarize tracking data per id
# summarizeTrips    Sumarize tracking data per trip
# point_on_land     Check if location overlap with landmask
# timedif           Return time difference from a time series of consecutive records
# timedif.segment   Segment trips based on temporal threshold





#---------------------------------------------------------------------
# embc_vars         Estimate EMbC movement variables
#---------------------------------------------------------------------
embc_vars <- function(x){
  # x is a move object
  #
  # When we already have the EMbC parameters, there is no need to run the clustering.
  # this can be helpful for large datasets (eg simulations). However, we need to 
  # calculate the movement variables in the same way as EMbC does.
  # Speed is provided in m/s and turning angles in absolute radians.
  # See Garriga et al. 2016 https://doi.org/10.1371/journal.pone.0151984
  
  library(move)
  deg2rad <- function(deg) {(deg * pi) / (180)}
  
  # calculate speed
  speed <- unlist(lapply(speed(x),c, NA ))
  
  # calculate absolute turn angle
  turn <-unlist(lapply(turnAngleGc(x), function(x) c(NA, x, NA)))  # degrees
  turn <- abs(deg2rad(turn))
  
  # replace NA by 0
  speed[is.na(speed)] <- 0
  turn[is.na(turn)] <- 0
  
  # return data.frame
  df <- data.frame(speed = as.numeric(speed), turn = as.numeric(turn))
}
#---------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
# filter_dup     Function to filter near-duplicate positions
#----------------------------------------------------------------------------------------------
filter_dup  <- function (data, step.time = 2/60, step.dist = 0.001){
  # Returns a data.frame with filtered data, and the estimates of vmax and vmax loop from SDLfilter
  # vmax: value of the maximum of velocity using in km/h
  #
  # Previous name of function was: L02L1_sdl
  
  require(SDLfilter)
  
  
  ## Keep original number of locations
  loc0 <- nrow(data)
  
  ## Convert standard format to SDLfilter
  
  # Standardize Location clasess
  data$lc <- as.character(data$lc)
  data$lc[data$lc == "A"] <- -1 
  data$lc[data$lc == "B"] <- -2
  data$lc[data$lc == "Z"] <- -9
  data$lc[data$lc == "G"] <- 4
  data$lc <- as.numeric(data$lc)
  
  # Rename columns
  names(data)[names(data)=="date"] <- "DateTime"
  names(data)[names(data)=="lc"] <- "qi"
  
  ### Remove duplicated locations, based on both time and space criteria
  data <- dupfilter(data.frame(data), step.time=step.time, step.dist=step.dist, conditional = FALSE)
  
  ## Back transform data.frame to standar format
  
  # Rename columns
  names(data)[names(data)=="DateTime"] <- "date"
  names(data)[names(data)=="qi"] <- "lc"
  
  # Standardize Location clasess
  data$lc[data$lc == -1] <- "A" 
  data$lc[data$lc == -2] <- "B"
  data$lc[data$lc == 4] <- "G" 
  
  ## Prepare output
  return(data)
}
#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
# filter_speed     Function to filter speed using SDLfilter
#----------------------------------------------------------------------------------------------
filter_speed  <- function (data, vmax = 10.8, method = 1){
  # Returns a data.frame with filtered data, and the estimates of vmax and vmax loop from SDLfilter
  # vmax: value of the maximum of velocity using in km/h
  #
  # Previous name of function was: L02L1_sdl
  
  require(SDLfilter)
  
  ## Keep original number of locations
  loc0 <- nrow(data)
  
  ## Convert standard format to SDLfilter
  
  # Standardize Location clasess
  data$lc <- as.character(data$lc)
  data$lc[data$lc == "A"] <- -1 
  data$lc[data$lc == "B"] <- -2
  data$lc[data$lc == "Z"] <- -9
  data$lc[data$lc == "G"] <- 4
  data$lc <- as.numeric(data$lc)
  
  # Rename columns
  names(data)[names(data)=="date"] <- "DateTime"
  names(data)[names(data)=="lc"] <- "qi"
  
  
  ## Filter out values above speed threshold, considering both previous and subsequent positions
  data <- ddfilter.speed(data, vmax = vmax, method = method)
  
  ## Back transform data.frame to standar format
  
  # Rename columns
  names(data)[names(data)=="DateTime"] <- "date"
  names(data)[names(data)=="qi"] <- "lc"
  
  # Standardize Location clasess
  data$lc[data$lc == -1] <- "A" 
  data$lc[data$lc == -2] <- "B"
  data$lc[data$lc == 4] <- "G" 
  
  ## Prepare output
  return(data)
}
#----------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------
# filter_track     Function to convert L0 to L1 location using SDLfilter
#----------------------------------------------------------------------------------------------
filter_track  <- function (data, vmax = 10.8, step.time = 5/60, step.dist = 0.001, method = 1){
  # Returns a data.frame with filtered data, and the estimates of vmax and vmax loop from SDLfilter
  # vmax: value of the maximum of velocity using in km/h
  #
  # Previous name of function was: L02L1_sdl
  
  require(SDLfilter)
  
  
  ## Keep original number of locations
  loc0 <- nrow(data)
  
  ## Convert standard format to SDLfilter
  
  # Standardize Location clasess
  data$lc <- as.character(data$lc)
  data$lc[data$lc == "A"] <- -1 
  data$lc[data$lc == "B"] <- -2
  data$lc[data$lc == "Z"] <- -9
  data$lc[data$lc == "G"] <- 4
  data$lc <- as.numeric(data$lc)
  
  # Rename columns
  names(data)[names(data)=="date"] <- "DateTime"
  names(data)[names(data)=="lc"] <- "qi"
  
  ### Remove duplicated locations, based on both time and space criteria
  data_dup <- dupfilter(data.frame(data), step.time=step.time, step.dist=step.dist, conditional = FALSE)
  nloc_dup <- nrow(data) - nrow(data_dup)
  
  ## Filter out Z location classess
  data_z <- filter(data_dup, qi > -9)
  nloc_z <- nrow(data_dup) - nrow(data_z) 
  
  ## Filter out values above speed threshold, considering both previous and subsequent positions
  data_speed <- ddfilter.speed(data_z, vmax = vmax, method = method)
  nloc_speed <- nrow(data_z) - nrow(data_speed)
  
  ## Estimate vmax from data
  data <- data_speed
  #V <- est.vmax(data, qi = 1)

  ## Back transform data.frame to standar format
  
  # Rename columns
  names(data)[names(data)=="DateTime"] <- "date"
  names(data)[names(data)=="qi"] <- "lc"
  
  # Standardize Location clasess
  data$lc[data$lc == -1] <- "A" 
  data$lc[data$lc == -2] <- "B"
  data$lc[data$lc == 4] <- "G" 
  
  ## Create a data.frame with processing report data
  proc <- data.frame(id = data$id[1],
                     trip = data$trip[1],
                     nloc_L0 = loc0,
                     removed_dup = nloc_dup,
                     removed_z = nloc_z,
                     removed_vmax = nloc_speed,
                     #estimated_vmax_kmh = V,
                     nloc_L1 = nrow(data),
                     percent_filtered = round(((loc0-nrow(data))/loc0)*100, 2))
  
  ## Prepare output
  out <- list(data = data, proc = proc)
  return(out)
}
#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
# landMask    Funcion to check if a track falls into the oceanMask
#----------------------------------------------------------------------------------------------
## Create function to check if point is on land
## It requires same inputs than gshhsMask()
landMask <- function(tm, pt){
  xy <- cbind(pt[[1]],pt[[2]])
  ov <- extract(oceanmask, xy)
  if(is.na(ov)) ov <- 1  # if point is outside study area classify as land (1)
  ov == 0  # returns a logical indicating whether the point is at sea (TRUE) or on land (FALSE)
}
#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
# segmentOnPort       Segment trips based on overlap with ports
#----------------------------------------------------------------------------------------------
segmentOnPort <- function(x){
  # x       vector with onport values
  
  # calculate difference between consecutive points
  outPort <- c(NA, x[-1] - x[-length(x)])
  
  # get breakpoints based on difference in port status
  breaks <- c(0, which(abs(outPort) > 0), length(x)+1)
  
  # split into segments
  intervals <- cut(seq(1:length(x)), breaks, right=FALSE)
  segments <- as.numeric(intervals)
  return(segments)
}
#----------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# SToverlap       Spatio-temporal overlap between tracks
#------------------------------------------------------------------------------------
SToverlap <- function(real_track, sim_track, exclude_dur, sp_thrs, t_thrs){
  
  require(geosphere)
  
  # filter data based from time from first location
  first_loc <- first(real_track$date)
  real_track <- dplyr::filter(real_track, date > (first_loc + exclude_dur))
  sim_track <- dplyr::filter(sim_track, date > (first_loc + exclude_dur))
  
  ST_overlap <- vector()
  
  for(i in 1:nrow(real_track)){
    
    # select real track data at time i
    itime <- real_track$date[i]
    ilon <- real_track$lon[i]
    ilat <- real_track$lat[i]
    
    # filter simulated data within temporal threshold
    isim <- dplyr::filter(sim_track, date < (itime + t_thrs), date > (itime - t_thrs))
    
    # calculate distance
    idis <- distGeo(c(ilon, ilat), cbind(isim$lon, isim$lat)) 
    
    # check if there is any overlap
    ST_overlap[i] <- any(idis < sp_thrs)
  }
  
  # check if there is any overlap
  ov <- any(ST_overlap == T)
  
  # return vector of overlap
  return(ov)
}
#------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# summarizeId       Sumarize tracking data per id
#------------------------------------------------------------------------------------
summarizeId <- function(data){
  # data is a data.frame with all tracking data per species
  
  df <- data %>%
    dplyr::arrange(date) %>%  # order by date
    dplyr::group_by(id) %>%  # select group info
    dplyr::summarize(date_deploy = first(date),
              lon_deploy = first(lon),
              lat_deploy = first(lat),
              date_last = last(date),
              time_interval_min = round(median(as.numeric(difftime(tail(date, -1), head(date, -1), units="mins")))),
              n_loc = n()) %>%  # get first and last observations
    dplyr::mutate(duration_d = round(difftime(date_last, date_deploy, units="days")))  # calculate duration of the track
  
  return(df)
}
#------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# summarizeSim        Sumarize simulated tracking data per trip
#------------------------------------------------------------------------------------
summarizeSim <- function(data){
  # data is a data.frame with simulated tracking data
  
  
  df <- data %>%
    arrange(date) %>%  # order by date
    group_by(id, trip) %>%  # select group info
    summarize(n_sim = length(unique(nsim)))
  
  return(df)
}
#------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------
# summarizeTrips        Sumarize tracking data per trip
#------------------------------------------------------------------------------------
summarizeTrips <- function(data){
  # data is a data.frame with all tracking data per species
  
  library(geosphere)
  library(dplyr)
  
  df <- data %>%
    dplyr::arrange(date) %>%  # order by date
    dplyr::group_by(id, trip) %>%  # select group info
    dplyr::summarize(date_deploy = first(date),
              lon_deploy = first(lon),
              lat_deploy = first(lat),
              date_last = last(date),
              time_interval_h = median(as.numeric(difftime(tail(date, -1), head(date, -1), units="hours"))),
              distance_km = sum(distGeo(p1 = cbind(lon, lat)), na.rm=TRUE)/1000,  # segment distance
              n_loc = n()) %>%  # get first and last observations
    dplyr::mutate(duration_h = round(difftime(date_last, date_deploy, units="hours")))  # calculate duration of the track
  
  return(df)
}
#------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
# point_on_land    Check if location overlap with landmask
#----------------------------------------------------------------------------------------------
point_on_land <- function(lon, lat, land = NULL){
  
  require(sp)
  require(maptools)
  require(maps)
  
  # If no landmask provided, get continent map
  if(is.null(land)){
    land <- map("world", fill = TRUE, plot = FALSE)
    land <- map2SpatialPolygons(land, IDs=land$names,proj4string=CRS("+proj=longlat +ellps=WGS84"))
  }

  # Convert to spatial point
  xy <- cbind(lon,lat)
  pts <- SpatialPoints(xy, proj4string=land@proj4string)
  
  # Overlay points with continents
  ov <- over(pts, as(land,"SpatialPolygons"))  # returns NA when no overlap, and poly ID when there is an overlap
  onland <- !is.na(ov)  # returns TRUE when there is overalp, FALSE when not

  # Return output
  return(onland)
}
#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
# timedif       Return time difference from a time series of consecutive records
#----------------------------------------------------------------------------------------------
# The aim of this function is to calculate the time difference (in hours)
# between consecutive positions. This information can be useful to assess the
# suitability of time series for further analysis (eg. SSM)
#
# Details:
# For the first location it will return a NA
timedif <- function(x){
  # x       POSIXct class
  # order time series
  x <- x[order(x)]
  
  # calculate difference between time steps
  tdif <- c(NA,as.numeric(difftime(x[-1], x[-length(x)], units="hours" )))
  tdif <- round(tdif, 2)
  return(tdif)
}
#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
# timedif.segment       Segment trips based on temporal threshold
#----------------------------------------------------------------------------------------------
timedif.segment <- function(x, thrs){
  # x       date.time
  # thrs    time threshold criteria to split into segments
  
  # order time series
  x <- x[order(x)]
  
  # calculate difference between time steps
  tdif <- c(NA,as.numeric(difftime(x[-1], x[-length(x)], units="hours" )))
  
  # get breakpoints based on time threshold
  breaks <- c(0, which(tdif >= thrs), length(tdif)+1)
  
  # split into segments
  intervals <- cut(order(x), breaks, right=FALSE)
  segments <- as.numeric(intervals)
  return(segments)
}
#----------------------------------------------------------------------------------------------
