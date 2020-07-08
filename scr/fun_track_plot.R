#-----------------------------------------------------------------------------------------
# fun_track_plot.R            Suite of common functions for plotting tracking data
#-----------------------------------------------------------------------------------------
# diffTimeHisto       Time diff histo
# map_argos           Map Argos locations
# mapForaging         Map locations by foraging
# mapL1               Map locations using L1 product
# mapSimTracks        Plot simulations and observed track
# plotEnviroTS        Plot time series of environmental data
# two.y               Plot tow-y axis function


#-----------------------------------------------------------------
# diffTimeHisto       Time diff histo
#-----------------------------------------------------------------
diffTimeHisto <- function(data, vline=24){
  
  # calculate time difference
  data$timedif <- timedif(data$date)
  
  # plot
  p <- ggplot(data = data, aes(x=timedif, y = ..count..)) +
    geom_histogram(breaks = c(0, 2,4,6, 12, 24, 48, 72), fill ="#4271AE") + 
    scale_x_continuous(name = "Time difference (hours)",
                       breaks = c(0, 2,4,6, 12, 24, 48, 72),
                       limits=c(0, 72)) +
    geom_vline(xintercept = vline, colour="red")
  
  return(p)
}
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# diffTimeHistoGPS       Time diff histo
#-----------------------------------------------------------------
diffTimeHistoGPS <- function(data, vline=0.08){
  
  # calculate time difference
  data$timedif <- timedif(data$date)
  
  # plot
  p <- ggplot(data = data, aes(x=timedif, y = ..count..)) +
    geom_histogram(breaks = seq(0, 0.5, 0.02), fill ="#4271AE") + 
    scale_x_continuous(name = "Time difference (hours)",
                       breaks = seq(0, 0.5, 0.02),
                       limits=c(0, 0.5)) +
    geom_vline(xintercept = vline, colour="red")
  
  return(p)
}
#-----------------------------------------------------------------



#-----------------------------------------------------------------
# map_argos       Map Argos locations
#-----------------------------------------------------------------
map_argos <- function (data){
  
  # Load libraries and dependencies
  library(ggplot2)
  # source("R/config.R")  # set your Google Drive data folder here
  # source("R/database_tools.R")
  
  # Import world map
  data(countriesHigh, package = "rworldxtra", envir = environment())
  wm <- suppressMessages(fortify(countriesHigh))
  
  ### Get metadata
  sdate <- data$date[1]
  edate <- data$date[nrow(data)]
  days <- round(as.numeric(difftime(edate, sdate, units="days")))
  
  ### Define extension for plot
  xl <- extendrange(data$lon, f = 0.2)
  yl <- extendrange(data$lat, f = 0.2)
  
  ### Plot
  p <- ggplot() +
    geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"),
                 fill = grey(0.3)) +
    coord_quickmap(xlim = xl, ylim = yl, expand = TRUE) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_point(data = data,
               aes_string(x = "lon", y = "lat", group = NULL, colour = "lc"),
               size = 2) +
    geom_path(data = data,
              aes_string(x = "lon", y = "lat", group = NULL)) +
    labs(title = paste(data$sp_code[1], "Id:", data$id[1]),
         subtitle = paste("Start:", sdate, "End:", edate, "(", days, "days)")) 
  
  
  return(p)
  
}
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# mapForaging      Map locations by foraging
#-----------------------------------------------------------------
mapForaging <- function (data, forag){
  
  # Load libraries and dependencies
  library(ggplot2)

  # Import world map
  data(countriesHigh, package = "rworldxtra", envir = environment())
  wm <- suppressMessages(fortify(countriesHigh))

  ### Get metadata
  sdate <- data$date[1]
  edate <- data$date[nrow(data)]
  days <- round(as.numeric(difftime(edate, sdate, units="days")))
  
  ### Define extension for plot
  xl <- extendrange(data$lon, f = 0.2)
  yl <- extendrange(data$lat, f = 0.2)
  
  ### Plot
  p <- ggplot() +
    geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"),
                 fill = grey(0.3)) +
    coord_quickmap(xlim = xl, ylim = yl, expand = TRUE) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_point(data = forag,
               aes_string(x = "lon", y = "lat", group = NULL, colour = "forag"),
               size = 2, color="red") +
    # geom_point(data = filter(data, forag==1),
    #            aes_string(x = "lon", y = "lat", group = NULL, colour = "forag"),
    #            size = 2) +
    # scale_colour_manual(values = c("red")) +
    geom_path(data = data,
              aes_string(x = "lon", y = "lat", group = NULL)) +
    ggtitle(paste("ID", data$id[1]))
  
  return(p)
  
}
#-----------------------------------------------------------------



#-----------------------------------------------------------------
# mapL1       Map locations using L1 product
#-----------------------------------------------------------------
mapL1 <- function (data){
  # data: locations L1 data.frame
  #
  # Required columns for L1
  # date: POSIXct
  # argosfilter:
  # onland:
  # lon:
  # lat:
  #
  # Function return a plot
  
  
  # Load libraries and dependencies
  library(ggplot2)
  library(RColorBrewer)
  #source("R/config.R")  # set your Google Drive data folder here
  #source("R/database_tools.R")
  
  # Import world map
  data(countriesHigh, package = "rworldxtra", envir = environment())
  wm <- suppressMessages(fortify(countriesHigh))
  
  ### Parse date format
  data$time <- as.integer(data$date)

  ### Filter location data
  #data <- filter(data, argosfilter == "not" & onland == "FALSE")
  
  ### Get metadata
  sdate <- data$date[1]
  edate <- data$date[nrow(data)]
  days <- round(as.numeric(difftime(edate, sdate, units="days")))
  
  ### Define extension for plot
  xl <- extendrange(data$lon, f = 0.5)
  yl <- extendrange(data$lat, f = 0.5)
  b <- c(data$time[1], data$time[nrow(data)])
  blab <- c(as.Date(data$date[1]), as.Date(data$date[nrow(data)]))
  
  ### Plot
  p <- ggplot() +
    geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"),
                 fill = grey(0.3)) +
    coord_quickmap(xlim = xl, ylim = yl, expand = TRUE) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_point(data = data,
               aes_string(x = "lon", y = "lat", group = NULL, colour = "time"),
               size = 2) +
    #scale_colour_gradientn(colours = terrain.colors(10)) + 
    scale_colour_gradientn(colours=rev(brewer.pal(10,"RdYlGn")),
                           breaks=b,labels=format(blab)) +
    #scale_colour_brewer() +
    geom_path(data = data,
              aes_string(x = "lon", y = "lat", group = "id")) +
    labs(title = paste(data$sp_code[1], "Id:", data$id[1]),
       subtitle = paste("Start:", sdate, "End:", edate, "(", days, "days)")) 
  
  return(p)
  
}
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# mapSimTracks       Plot simulations and observed track
#-----------------------------------------------------------------
mapSimTracks  <- function (simData, obsData, title = NULL){
  
  # Load libraries and dependencies
  library(ggplot2)
  
  # Import world map
  data(countriesHigh, package = "rworldxtra", envir = environment())
  wm <- suppressMessages(fortify(countriesHigh))
  
  ### Define extension for plot
  xl <- extendrange(c(simData$lon, obsData$lon), f = 0)
  yl <- extendrange(c(simData$lat, obsData$lat), f = 0)
  
  ### Plot
  p <- ggplot() +
    geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"),
                 fill = grey(0.3)) +
    coord_quickmap(xlim = xl, ylim = yl, expand = TRUE) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_path(data = simData,
              aes_string(x = "lon", y = "lat", group = "simid"),
              size=0.5, alpha=0.5, color="dodgerblue3") +
    geom_path(data = obsData,
              aes_string(x = "lon", y = "lat", group = "trip"),
              size=0.5, alpha=1, color="red") +
    geom_point(data = data.table::first(obsData),
               aes_string(x = "lon", y = "lat"),
               size=3, alpha=1, color="green") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    ggtitle(title)
  
  return(p)
}
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# plotEnviroTS        Plot time series of environmental data
#-----------------------------------------------------------------
plotEnviroTS <- function(data, vars){
  
  # Load libraries and dependencies
  library(ggplot2)
  library(reshape2)
  
  # melt data frame
  data_melt <- melt(data, id.vars <- "date", measure.vars = vars)
  
  # plot facet
  p <- ggplot(data_melt, aes(x=date, y=value, colour=variable)) + 
    geom_path() + 
    facet_grid(variable ~.,
               scale = "free_y") +
    theme(legend.position="none") +
    labs(title = paste("Id:", data$id[1])) 
  
  return(p)
}
#-----------------------------------------------------------------



#------------------------------------------------------------------------
# two.y               Plot tow-y axis function
#------------------------------------------------------------------------
two.y <- function(x, y1, y2,
                  y1.lim = range(y1),
                  y2.lim = range(y2),
                  y1.lab = "mean",
                  y2.lab = "SD",
                  x.lab = "Number of simulations",
                  var.name = "",
                  ttxt = NULL,
                  ...) {
  
  # This functions has been adapted from: https://github.com/kjhealy/two-y-axes
  
  # Set min and max for both variables
  # round values with 1 decimal
  #decimals_y1 <- max(nchar(sub(".*\\.(0*).*","\\1",y1)))+2
  #decimals_y2 <- max(nchar(sub(".*\\.(0*).*","\\1",y2)))+2
  dif_y1 <- max(y1)-min(y1)
  dif_y1 <- format(dif_y1, scientific=F)
  decimals_y1 <- nchar(sub(".*\\.(0*).*","\\1",dif_y1))+2
  dif_y2 <- max(y2)-min(y2)
  dif_y2 <- format(dif_y2, scientific=F)
  decimals_y2 <- nchar(sub(".*\\.(0*).*","\\1",dif_y2))+2
  y1.lo <- round(y1.lim[1], decimals_y1)
  y1.hi <- round(y1.lim[2], decimals_y1)
  y2.lo <- round(y2.lim[1], decimals_y2)
  y2.hi <- round(y2.lim[2], decimals_y2)
  
  # plot first variable on left y axis
  par(mar=c(5,6,4,5)+.1, las=1)
  plot(x, y1,
       type="l",
       col="#00AFBB",
       lwd=2,
       xlab= x.lab,
       ylab="",
       yaxt = "n",
       xaxt="n")
  
  # define x and y axis
  axis(side = 1, at = seq(0, max(x), 10))
  axis(side = 2, at = c(y1.lo, y1.hi))
  
  # plot vertical lines
  abline(v=seq(0, max(x), 10), col=c("grey"), lty=c(1), lwd=c(0.2))
  
  # prepare second variable
  par(new=TRUE)
  
  plot(x, y2, type="l",
       col="#E7B800",
       lwd=2,
       xaxt="n",
       yaxt="n",
       xlab="",
       ylab="")
  
  # define y axis
  axis(side = 4, at = c(y2.lo, y2.hi))
  title(main = ttxt)
  
  # plot ylabels
  par(las=0)
  mtext(y1.lab, side=2, line=2)
  mtext(var.name, side=2, line=4)
  mtext(y2.lab, side=4, line=2)
  
  # plot legend
  legend("bottomright",
         col=c("#00AFBB","#E7B800"),
         bty="n", lty=1, lwd=2,
         legend=c("mean", "SD"),
         horiz = TRUE, inset=c(0,1), xpd=TRUE)
}
#------------------------------------------------------------------------
