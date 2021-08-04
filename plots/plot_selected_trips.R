#--------------------------------------------------
# plot_selected_trips
#--------------------------------------------------

source("setup.R")
source("scr/fun_track_reading.R")  # read multiple tracking data formats
library(scico)

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

sp_code <- "GAZ"

# set output directory
outdir <- paste(output_data, "fig/selected_trips/", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import list of selected trips
trips_xls <- paste0(input_data, "/cardona/TracksSelected.xlsx")
trips <- read_excel(trips_xls, sheet="SelTrips")
# prepare date time formats
trips <- trips %>%
  dplyr::mutate(date_start = format(date_start, "%Y-%m-%d"),
         date_end = format(date_end, "%Y-%m-%d"),
         time_start = format(time_start, "%H:%M:%S"),
         time_end = format(time_end, "%H:%M:%S"),
         dateTimeStart = parse_date_time(paste(date_start, time_start), "Ymd HMS"),
         dateTimeEnd = parse_date_time(paste(date_end, time_end), "Ymd HMS"),
         tripID = paste("trip", id, format(dateTimeStart, "%Y%m%d"), sep="_")) %>%
  dplyr::select(id, tripID, tripType, dateTimeStart, dateTimeEnd)


# import animal tracks
ssm_dir <- paste0(output_data, "/tracking/", sp_code, "/L1_locations")
ssm_files <- list.files(ssm_dir, full.names = T, pattern = "L1_locations.csv")
ssm <- readTrack(ssm_files)


# select individual trips
# for each trip, select data and append
trip_list <- list()
for(i in 1:nrow(trips)){
  ssm_sel <- ssm %>%
    filter(id == trips$id[i], date >= trips$dateTimeStart[i], date <= trips$dateTimeEnd[i]) %>%
    mutate(tripType = trips$tripType[i],
           tripID = trips$tripID[i])
  trip_list[[i]] <- ssm_sel
}

trip_data <- rbindlist(trip_list)


# add month
#ssm$month <- month(ssm$date) %>% as.factor()

# import bathymetry
bat <- raster(paste0(output_data, "/stack_daily/2019/02/20190201_enviro.grd"))
batdf <- as.data.frame(bat,xy=TRUE)%>%drop_na()

# create hillshade
slope <- terrain(bat, opt='slope')
aspect <- terrain(bat, opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)
hilldf <- as.data.frame(hill, xy=TRUE) %>% drop_na()


# landmask
world <- ne_countries(scale = "medium", returnclass = "sf")
e <- extent(-90, -20, -80, -50)
box = c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4])
land <- st_crop(world, box)



#---------------------------------------------------------------
# 2. Plot (all trips)
#---------------------------------------------------------------

# labels <- ssm %>%
#   distinct(id) %>%
#   mutate(label = paste0("#", id))


trips <- unique(trip_data$tripID)

for(i in 1:length(trips)){
  
  ssm <- filter(trip_data, tripID == trips[i])
  
  
  labels <-ssm %>%
    distinct(id) %>%
    mutate(label = paste0("#", id))
  
  # define xy limits
  xl <- range(ssm$lon)+c(-1,1)
  yl <- range(ssm$lat)+c(-1,1)
  
  
  ### define lon/lat bounds
  # range from data
  xl <- range(ssm$lon)
  yl <- range(ssm$lat)
  # get centroid
  zoom_to <- c(mean(xl), mean(yl))  # center of the range
  # define zoom level
  lon_span <- xl[2]-xl[1]
  lat_span <- yl[2]-yl[1]
  zoom_lon <- floor(log2(360/lon_span))
  zoom_lat <- floor(log2(180/lat_span))
  zoom_level <- min(zoom_lon, zoom_lat) -1
  # define span
  lon_span <- 360 / 2^zoom_level
  lat_span <- 180 / 2^zoom_level
  # define boundaries
  lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
  lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

  # plot
  p <- ggplot()+
    # lines and points
  geom_path(data = ssm, 
            aes(x=lon,y=lat,group=tripID, color=id), size=0.8,  # "#3182bd"
            alpha = 1) +
    # land mask
    geom_sf(fill=grey(0.7), colour = grey(0.6), size = 0.1, data=land) +
    # spatial domain
    #coord_sf(xlim = xl, ylim = yl) +
    coord_sf(xlim = lon_bounds, ylim = lat_bounds, expand = F, ndiscr = 1000) +
    #, expand = F, ndiscr = 1000) +
    labs(x='',y='') +
    # facet per individual
    #facet_grid(tripID~., scales="free") +
    # labels and images on individual facets
    #geom_text(data=labels, aes(label=label), vjust = "inward", hjust = "inward") +
    annotate("text",label=labels$label, x = -Inf, y = Inf, hjust = -0.3, vjust = 1.5) +
    # theme
    theme_bw() +
    theme(
      legend.position='none',
      panel.grid = element_blank(),
      strip.text.x = element_blank(),
      strip.background = element_blank())
  
  # Export plot
  p_png <- paste0(outdir, "/trip", i, ".png")
  ggsave(p_png, p, width=16, height=22, units="cm", dpi=300)
  
}



#---------------------------------------------------------------
# 3. Plot (final selection)
#---------------------------------------------------------------

# list of selected trips
sel_panel <- c("trip_64519_20190228", "trip_64491_20190307", "trip_64492_20190313",
         "trip_64487_20190404", "trip_64490_20190515", "trip_64519_20190421")

# create empty list for plots
plot_list <- list()


for(i in 1:length(sel_panel)){
  
  # select trip
  itrip <- sel_panel[i]
  ssm <- filter(trip_data, tripID == itrip)
  
  # label id
  labels <-ssm %>% distinct(id) %>% mutate(label = paste0("#", id))
  
  # define xy limits
  xl <- range(ssm$lon)
  yl <- range(ssm$lat)
  
  #  define zoom level
  lon_span <- xl[2]-xl[1]
  lat_span <- yl[2]-yl[1]
  
  lon_bounds <- range(ssm$lon) + c(-4,4)
  lat_bounds <- range(ssm$lat)  + c(-1,1)

  if(i == 1){
    lon_bounds <- range(ssm$lon) + c(-2,2)
    lat_bounds <- range(ssm$lat)+ c(-0.2,0.2)
  }
  
  
  # plot
  p <- ggplot()+
    # land mask
    geom_sf(fill=grey(0.7), colour = grey(0.6), size = 0.1, data=land) +
    # lines
    geom_path(data = ssm, 
              aes(x=lon,y=lat,group=tripID), color="black", size=0.8,  # "#3182bd"
              alpha = 1) +
    # points
    geom_point(data = ssm[1,], 
              aes(x=lon,y=lat), color="#1A85FF", fill="white", size=1.5, shape=24, stroke=1.5, # "#3182bd"
              alpha = 1) +
    geom_point(data = ssm[nrow(ssm),], 
               aes(x=lon,y=lat), color="#D41159", fill="white", size=1.5, shape=22, stroke=1.5, # "#3182bd"
               alpha = 1) +
    # spatial domain
    #coord_sf(xlim = xl, ylim = yl) +
    coord_sf(xlim = lon_bounds, ylim = lat_bounds, expand = T, ndiscr = 1000) +
    #, expand = F, ndiscr = 1000) +
    labs(x='',y='') +
    # facet per individual
    #facet_grid(tripID~., scales="free") +
    # labels and images on individual facets
    #geom_text(data=labels, aes(label=label), vjust = "inward", hjust = "inward") +
    annotate("text",label=labels$label, x = -Inf, y = Inf, hjust = -0.3, vjust = 1.5) +
    # theme
    theme_bw() +
    theme(
      legend.position='none',
      panel.grid = element_blank(),
      strip.text.x = element_blank(),
      strip.background = element_blank())
  
  plot_list[[i]] <- p
  
}



#creo els 4 panels on els 3 primers panels son 3 mapes fets amb el boundigbox que has creat tu i el 4rt panel hi poso "NULL" per a que quedi buit l'espai.
composite <- ggpubr::ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                               plot_list[[4]], plot_list[[5]], plot_list[[6]],
                            labels  = c("a","b","c","d", "e", "f"),
                            ncol = 2, nrow = 3,
                            align="hv")

p_png <- paste0(outdir, "composite.png")
ggsave(p_png, composite, width=22, height=22, units="cm", dpi=300)




