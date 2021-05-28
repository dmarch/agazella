#--------------------------------------------------
# plot_individual_tracks
#--------------------------------------------------

source("setup.R")
source("scr/fun_track_reading.R")  # read multiple tracking data formats
library(scico)



mod_code <- "brt"
sp_code <- "GAZ"

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste(output_data, "habitat-model", sp_code, mod_code, "predict", sep="/")
outdir <- paste(output_data, "fig", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import animal tracks
ssm_dir <- paste0(output_data, "/tracking/", sp_code, "/L1_locations")
ssm_files <- list.files(ssm_dir, full.names = T, pattern = "L1_locations.csv")
ssm <- readTrack(ssm_files)

# add month
ssm$month <- month(ssm$date) %>% as.factor()

# import bathymetry
#bat <- raster(paste0(output_data, "/stack_daily/2019/02/20190201_enviro.grd"))
#batdf <- as.data.frame(bat,xy=TRUE)%>%drop_na()

# import bathymetry
bathync <- paste0(input_data, "/bathymetry/GEBCO_2014_2D.nc")#"E:/data/gebco/RN-4015_1510486680263/GEBCO_2014_2D.nc"
bathy <- raster(bathync)
# crop to larger region of the study area 
e <- extent(-90, -20, -80, -40)
bathy <- crop(bathy, e)
# resample to a coarser resolution (0.042 x 0.042 degrees)
bathy <- aggregate(bathy, fact = 10, fun = mean)

# set values above 0 as NA
bathy[bathy >= 0] <- NA
batdf <- as.data.frame(bathy,xy=TRUE)%>%drop_na()

# create hillshade
slope <- terrain(bathy, opt='slope')
aspect <- terrain(bathy, opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)
hilldf <- as.data.frame(hill, xy=TRUE) %>% drop_na()


# landmask
#world <- ne_countries(scale = "medium", returnclass = "sf")
e <- extent(-90, -20, -80, -50)
box = c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4])
#land <- st_crop(world, box)
# set path to natural earth data
ne_land_shp <- paste0(input_data, "/ne/ne_10m_land/ne_10m_land.shp")
ne_ice_shp <- paste0(input_data, "/ne/ne_10m_antarctic_ice_shelves_polys/ne_10m_antarctic_ice_shelves_polys.shp")
# import ne data
land <- st_read(ne_land_shp)
ice <- st_read(ne_ice_shp)
# crop ne data
land <- st_crop(land, box)
ice <- st_crop(ice, box)
# crop south america
e <- extent(-80, -50, -60, -50)
box = c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4])
land_sam <- st_crop(land, box)


#---------------------------------------------------------------
# 2. Plot
#---------------------------------------------------------------

# define xy limits
e <- extent(-90, -20, -80, -50)
xl <- c(e@xmin, e@xmax)#range(ssm$lon)
yl <- c(-75, e@ymax)#range(ssm$lat)

# legend breaks
min_month <- floor_date(min(ssm$date), unit="month")
max_month <- ceiling_date(max(ssm$date), unit="month")
breaks <- seq.POSIXt(from = min_month, to = max_month, by="month")



##### Main plot
p <- ggplot()+
  # plot hillshade
  #geom_raster(aes(x=x, y=y, alpha=layer),data=hilldf)+
  #scale_alpha(range =  c(0, 1), guide = "none") +
  # plot bathymetry
  geom_raster(aes(x=x,y=y,fill=Elevation.relative.to.sea.level), alpha=0.7, data=batdf) +
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
  geom_sf(fill=grey(0.7), colour = grey(0.7), size = 0.1, data=land, alpha=0.9)+
  geom_sf(fill="white", colour = grey(0.7), size = 0.1, data=ice, alpha=0.9) +
  #geom_sf(fill=grey(0.7), colour = grey(0.3), size = 0.1, data=land_sam, alpha=0.9) +
  # spatial domain
  coord_sf(xlim = xl, ylim = yl, expand=c(0, 0)) +
  labs(x='',y='') +
  # annotation of toponyms
  annotate(geom = "text", x = -30.5, y = -54, label = "South Georgia Is.",
    fontface = "plain", color = "black", size = 3.5) +
  annotate(geom = "text", x = -40, y = -61, label = "South Orkney Is.",
           fontface = "plain", color = "black", size = 3.5) +
  annotate(geom = "text", x = -65, y = -61.5, label = "South Shetland Is.",
           fontface = "plain", color = "black", size = 3.5) +
  annotate(geom = "text", x = -45, y = -57.5, label = "SCOTIA SEA",
           fontface = "plain", color = "black", size = 3.5) +
  annotate(geom = "text", x = -59, y = -65.5, label = "ANTARCTIC PENINSULA",
           fontface = "plain", color = "black", size = 3.5, angle = 45) +
  # annotate(geom = "text", x = -68.5, y = -69, label = "Marguerite \n Bay",
  #          fontface = "plain", color = "black", size = 3) +
  annotate(geom = "text", x = -80, y = -65, label = "BELLINGSHAUSEN \n SEA",
           fontface = "plain", color = "black", size = 3.5) +
  annotate(geom = "text", x = -45, y = -67, label = "WEDDELL \n SEA",
           fontface = "plain", color = "black", size = 3.5) +
  # theme
  theme_bw(base_size = 14) +
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
p_png <- paste0(outdir, "/all-tracks.png")
ggsave(p_png, p, width=22, height=18, units="cm", dpi=300)


