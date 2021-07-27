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
outdir <- paste(output_data, "fig", sep="/")
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
         dateTimeEnd = parse_date_time(paste(date_end, time_end), "Ymd HMS")) %>%
  dplyr::select(id, tripType, dateTimeStart, dateTimeEnd)


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
           tripID = i)
  trip_list[[i]] <- ssm_sel
}

ssm <- rbindlist(trip_list)


# add month
ssm$month <- month(ssm$date) %>% as.factor()

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
# 2. Plot
#---------------------------------------------------------------

labels <- ssm %>%
  distinct(id) %>%
  mutate(label = paste0("#", id))

# define xy limits
xl <- range(ssm$lon)
yl <- range(ssm$lat)

# legend breaks
min_month <- floor_date(min(ssm$date), unit="month")
max_month <- ceiling_date(max(ssm$date), unit="month")
breaks <- seq.POSIXt(from = min_month, to = max_month, by="month")



# plot
p <- ggplot()+
  # plot hillshade
  geom_raster(aes(x=x, y=y, alpha=layer),data=hilldf)+
  scale_alpha(range =  c(0, 1), guide = "none") +
  # plot bathymetry
  geom_raster(aes(x=x,y=y,fill=BAT), alpha=0.8, data=batdf)+
  scale_fill_gradient(low=brewer.pal(9,"Blues")[5], high=brewer.pal(9,"Blues")[1],
                      name = "Depth (m)",
                      guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
                                              frame.colour="black", frame.linewidth=1, frame.linetype=1,
                                              ticks.colour = "black", ticks = TRUE)) +
  # lines and points
  geom_path(data = ssm, 
            aes(x=lon,y=lat,group=id, color=date), size=0.8,  # "#3182bd"
            alpha = 1)+
  scale_color_scico(name = "Date",
                    palette = "lajolla", direction = -1, trans="time",
                    breaks = breaks,
                    labels = label_date(format = "%b"),
                    guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
                                            frame.colour="black", frame.linewidth=1, frame.linetype=1,
                                            ticks.colour = "black", ticks = TRUE)) +
  # land mask
  #geom_sf(fill=grey(0.7), colour = grey(0.6), size = 0.1, data=land)+
  # spatial domain
  coord_sf(xlim = xl, ylim = yl, expand=c(0.02, 0.02)) +
  labs(x='',y='') +
  # facet per individual
  facet_wrap(tripType~., ncol = 3)+
  # labels and images on individual facets
  #geom_text(data=labels, aes(x=-43, y=-69, label=label)) +
  # theme
  theme_bw() +
  theme(
    legend.position='bottom',
    legend.direction = "horizontal",
    legend.box="horizontal",
    legend.title = element_text(size = 10),
    legend.key.height=grid::unit(0.4,"cm"),
    legend.key.width=grid::unit(1,"cm"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank())

# Export plot
p_png <- paste0(outdir, "/individual-trips.png")
ggsave(p_png, p, width=16, height=22, units="cm", dpi=300)


