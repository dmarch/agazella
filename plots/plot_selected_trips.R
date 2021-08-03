#--------------------------------------------------
# plot_individual_tracks
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
         tripID = paste(id, format(dateTimeStart, "%Y%m%d"), sep="_")) %>%
  dplyr::select(tripID, id, tripType, dateTimeStart, dateTimeEnd)


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


# check if no data for a given trip
which(!trips$tripID %in% unique(trip_data$tripID))

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
e <- extent(-100, -20, -80, -50)
box = c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4])
land <- st_crop(world, box)



#---------------------------------------------------------------
# 2. Plot
#---------------------------------------------------------------

# labels <- ssm %>%
#   distinct(id) %>%
#   mutate(label = paste0("#", id))


trips <- unique(trip_data$tripID)

for(i in 1:length(trips)){
  
  ssm <- filter(trip_data, tripID == trips[i])
  
  itrip <- trips[i]
  itrip_type <- paste("Trip type:", ssm$tripType[1])
  
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
  
  
  # legend breaks
  # min_month <- floor_date(min(ssm$date), unit="month")
  # max_month <- ceiling_date(max(ssm$date), unit="month")
  # breaks <- seq.POSIXt(from = min_month, to = max_month, by="month")
  
  
  
  # plot
  p <- ggplot()+
    # # plot hillshade
    # geom_tile(aes(x=x, y=y, alpha=layer),data=hilldf)+
    # scale_alpha(range =  c(0, 1), guide = "none") +
    # # plot bathymetry
    # geom_tile(aes(x=x,y=y,fill=BAT), alpha=0.8, data=batdf)+
    # scale_fill_gradient(low=brewer.pal(9,"Blues")[5], high=brewer.pal(9,"Blues")[1],
    #                     name = "Depth (m)",
    #                     guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
    #                                             frame.colour="black", frame.linewidth=1, frame.linetype=1,
    #                                             ticks.colour = "black", ticks = TRUE)) +
    # lines and points
    geom_path(data = ssm, 
            aes(x=lon,y=lat,group=tripID), color="black", size=0.8,  # "#3182bd"
            alpha = 1) +
    # land mask
    geom_sf(fill=grey(0.7), colour = grey(0.6), size = 0.1, data=land) +
    # points
    geom_point(data = ssm[1,], 
              aes(x=lon,y=lat), color= "darkgreen", fill="white", size=3, shape=24, stroke=2,  # "#3182bd"
              alpha = 1) +
    geom_point(data = ssm[nrow(ssm),], 
               aes(x=lon,y=lat), color= "red", fill="white", size=3, shape=22, stroke=2,  # "#3182bd"
               alpha = 1) +

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
    annotate("text",label=itrip_type, x = -Inf, y = Inf, hjust = -0.1, vjust = 2.7) +
    # theme
    theme_bw() +
    theme(
      legend.position='none',
      panel.grid = element_blank(),
      strip.text.x = element_blank(),
      strip.background = element_blank())
  
  # Export plot
  p_png <- paste0(outdir, "/trip_", itrip, ".png")
  ggsave(p_png, p, width=16, height=22, units="cm", dpi=300)
  
}




# define xy limits
xl <- range(trip_data$lon)+c(-1,1)
yl <- range(trip_data$lat)+c(-1,1)

xl[2] <- -50
yl[2] <- -60

# plot
p <- ggplot()+
  # # plot hillshade
  # geom_tile(aes(x=x, y=y, alpha=layer),data=hilldf)+
  # scale_alpha(range =  c(0, 1), guide = "none") +
  # # plot bathymetry
  # geom_tile(aes(x=x,y=y,fill=BAT), alpha=0.8, data=batdf)+
  # scale_fill_gradient(low=brewer.pal(9,"Blues")[5], high=brewer.pal(9,"Blues")[1],
  #                     name = "Depth (m)",
  #                     guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
  #                                             frame.colour="black", frame.linewidth=1, frame.linetype=1,
  #                                             ticks.colour = "black", ticks = TRUE)) +
  # lines and points
geom_path(data = trip_data, 
          aes(x=lon,y=lat, group=tripID, color=tripType), size=0.8,  # "#3182bd"
          alpha = 1) +
  # land mask
  geom_sf(fill=grey(0.7), colour = grey(0.6), size = 0.1, data=land) +
  # spatial domain
  #coord_sf(xlim = xl, ylim = yl) +
  coord_sf(xlim = xl, ylim = yl, expand = F, ndiscr = 1000) +
  #, expand = F, ndiscr = 1000) +
  labs(x='',y='') +
  # facet per individual
  #facet_grid(tripID~., scales="free") +
  # labels and images on individual facets
  #geom_text(data=labels, aes(label=label), vjust = "inward", hjust = "inward") +
  #annotate("text",label=labels$label, x = -Inf, y = Inf, hjust = -0.3, vjust = 1.5) +
  # theme
  theme_bw() +
  theme(
    legend.position='bottom',
    panel.grid = element_blank(),
    strip.text.x = element_blank(),
    strip.background = element_blank())

