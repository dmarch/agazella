##################################################################################################
# Title: utilsGeneral.R
# Abstract: Functions for general purposes
# Last update: 2014-4-8
# Author: David March
# Email: damamo82@gmail.com
#
# LIST OF FUNCTIONS
# beep                  Play a sound at the end of a script
# coverage              Calculate temporal overlap between time intervals
# getHistoricalWeather  Get historical weather data from wunderground
# rcover                Estimate coverage on grid from factor variables
# time.matrix           Create a matrix of time differences
# twitAlarm             Send direct message to twitter
##################################################################################################

#----------------------------------------------------------------------------------
# beep          Play a sound at the end of a script
#----------------------------------------------------------------------------------
#usage: beep()
#source: http://stackoverflow.com/questions/3365657/is-there-a-way-to-make-r-beep-play-a-sound-at-the-end-of-a-script
beep <- function(n = 3){
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(.5)
  }
}
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# coverage    Calculate temporal overlap between time intervals
#----------------------------------------------------------------------------------
# Arguments:
# S1          Starting time of first interval, numeric value (eg. use 9.5 for defining 9:30)
# S2          Ending time of first interval
# I1          Starting time of second interval
# I2          Ending time of second interval
# tolerance   Time tolerance, numeric value
coverage<-function(S1,S2,I1,I2,tolerance){
  surveys<-length(S1)
  interviews<-length(I1)
  overlap=rep(NA,length(I1))
  count=rep(NA,length(S1))
  for (s in 1:surveys){
    s1<-S1[s]
    s2<-S2[s]
    for (i in 1:interviews){
      i1<-I1[i]
      i2<-I2[i]
      #exclusion
      if (s2<=(i1+tolerance) | s1>=(i2-tolerance)){
        overlap[i]<-0
      } else {
        ini=ifelse(s1<i1,i1,s1)
        end=ifelse(s2<i2,s2,i2)  
        overlap[i]<-ifelse((end-ini)>=tolerance,1,0)
      }
    }
    count[s]<-(sum(overlap)/interviews)*100
  }
  return(count)
}
#-------------------------------------------------------------------------------------------------


#---------------------------------------------------------------
# getHistoricalWeather    Get historical weather data from wunderground
#---------------------------------------------------------------
# Arguments:
# airport.code    International airport code
# date            character string representing date in format %Y%m%d
# apikey          api key registered in wunderground
#
# Usage:
#   date.range <- seq.Date(from=as.Date('2006-1-05'), to=as.Date('2006-1-06'), by='1 day')
#   hdwd <- data.frame()
#   for(i in seq_along(date.range)) {
#     weather.data <- getHistoricalWeather('PMI', format(date.range[i], "%Y%m%d"),apikey)                 
#     hdwd <- rbind(hdwd, ldply(weather.data$history$dailysummary, 
#                            function(x) c('PMI', date.range[i], x$fog, x$rain, x$snow,  x$meantempi, x$meanvism, x$maxtempi, x$mintempi)))
#   }
#   colnames(hdwd) <- c("Airport", "Date", 'Fog', 'Rain', 'Snow','AvgTemp', 'AvgVisibility','MaxTemp','MinTemp','AvgPrecip')
#
# Note:
# Developer plan has a limit of 500 call per day with 10 calls per minut limit.
getHistoricalWeather <- function(airport.code="PMI", date="Sys.Date()",apikey){
  require(rjson)
  require(plyr)
  
  base.url <- paste('http://api.wunderground.com/api/',apikey,'/',sep="")
  # compose final url
  final.url <- paste(base.url, 'history_', date, '/q/', airport.code, '.json', sep='')
  
  
  # reading in as raw lines from the web service
  conn <- url(final.url)
  raw.data <- readLines(conn, n=-1L, ok=TRUE)
  # Convert to a JSON
  weather.data <- fromJSON(paste(raw.data, collapse=""))
  close(conn)
  return(weather.data)
}
#---------------------------------------------------------------


#----------------------------------------------------------------------------------
# time.matrix  Create a matrix of time differences
#----------------------------------------------------------------------------------
# Arguments:
# x    vector of Dates
#
# Note:
# Code adapted from http://stackoverflow.com/questions/17133217/create-a-distance-matrix-in-r-using-parallelization
#
# Example:
# See rac function to calculate time difference between surveys to calculate temporal residual autocovariate

time.matrix <- function(x){
  n <- length(x)                                #number of tweets
  xy <- matrix(nrow=n, ncol=n)                  #create NxN matrix
  colnames(xy) <- names(x)                      #set column
  rownames(xy) <- names(x)                      #and row names
  
  for(i in 1:n) {
    for (j in 1:n){
      xy[i,j]<-difftime(x[i],x[j],"days")
    }
  }
  return(xy)
}
#----------------------------------------------------------------------------------


#----------------------------------------------------------------------------------
# twitAlarm           Send direct message to twitter
#----------------------------------------------------------------------------------
# Arguments:
# cred          Credential object
# user          User to send the message
# test          Text of the message
twitAlarm<-function(cred,user,text){
  
  require(RCurl)
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  require(twitteR)
  load(cred)
  registerTwitterOAuth(twitCred)
  dmSend(text,user)
}
#----------------------------------------------------------------------------------
