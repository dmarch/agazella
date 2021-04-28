#--------------------------------------------------
# all-tracks-daily-color
#--------------------------------------------------

# for each year-month, locate the folder and files
# import tif files as stack
# average
# plot map
# raster facets: https://stackoverflow.com/questions/66389254/aligning-ggplot-faceted-raster-maps-with-a-single-map-separate-legends
# animation: https://www.r-bloggers.com/2020/10/climate-animation-of-maximum-temperatures/
# anima paths: https://hansenjohnson.org/post/animate-movement-in-r/

source("setup.R")
source("scr/fun_track_reading.R")  # read multiple tracking data formats

mod_code <- "brt"
sp_code <- "GAZ"

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste(output_data, "habitat-model", sp_code, mod_code, "predict", sep="/")
outdir <- paste(output_data, "tracking", sp_code, "plots", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import animal tracks
ssm_dir <- paste0(output_data, "/tracking/", sp_code, "/L1_locations")
ssm_files <- list.files(ssm_dir, full.names = T, pattern = "L1_locations.csv")
ssm <- readTrack(ssm_files)

# add month
ssm$month <- month(ssm$date) %>% as.factor()

# import bathymetry
bat <- raster(paste0(output_data, "/stack_daily/2019/02/20190201_enviro.grd"))
batdf <- as.data.frame(bat,xy=TRUE)%>%drop_na()



# landmask
world <- ne_countries(scale = "medium", returnclass = "sf")
e <- extent(-90, -20, -80, -50)
box = c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4])
land <- st_crop(world, box)


as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
  #format(as.Date(x, origin = '1970-01-01'), format = '%d %Y')
}





# plot
p <- ggplot()+
  
  geom_raster(aes(x=x,y=y,fill=BAT),data=batdf)+
  scale_fill_distiller(palette = "Greys") +


  
  # lines and points
  geom_path(data = dplyr::filter(ssm), 
            aes(x=lon,y=lat,group=id),  colour = "black",size=0.5,  # "#3182bd"
            alpha = 1)+
  geom_point(data = dplyr::filter(ssm),
            aes(x=lon,y=lat, colour = month), size=0.8,shape=19,
            alpha = 0.6)+
  scale_color_brewer(palette="Spectral", labels = month.abb[2:9]) +
  
  geom_sf(fill=grey(0.8), size = 0.2, data=land)+
  # 
  # geom_path(data = dplyr::filter(ssm), 
  #           aes(x=lon,y=lat,group=id),  colour = "black",
  #           alpha = 0.6)+
  # geom_point(data = ssm, 
  #            aes(x=lon,y=lat,group=trip, color=date),
  #            alpha = 0.7, shape=19, size = 1)+
  #scale_color_gradientn(colours = rainbow(5), labels=as.Date_origin) +
coord_sf(expand=c(0,0))+
  labs(x='Longitude',y='Latitude')+
  theme_article(base_size=16) #+
  # theme(legend.position = "bottom",
  #       legend.direction = "horizontal",
  #       legend.title = element_text(size = 15),
  #       legend.key.height=grid::unit(0.6,"cm"),
  #       legend.key.width=grid::unit(2,"cm"),
  #       legend.text = element_text(size = 13))+#,
  # #axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
  # #axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  # # legend title
  # guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, ticks = TRUE))


# Export plot
p_png <- paste0(outdir, "/all-tracks.png")
ggsave(p_png, p, width=18, height=20, units="cm", dpi=300)




### Zoom into Antarctic Peninsula

p <- ggplot()+
  
  geom_raster(aes(x=x,y=y,fill=BAT),data=batdf)+
  scale_fill_distiller(palette = "Greys") +
  

  
  # lines and points
  geom_path(data = dplyr::filter(ssm), 
            aes(x=lon,y=lat,group=id),  colour = "black",size=0.5,  # "#3182bd"
            alpha = 1)+
  geom_point(data = dplyr::filter(ssm),
             aes(x=lon,y=lat, colour = month), size=0.8,shape=19,
             alpha = 0.6)+
  scale_color_brewer(palette="Spectral", labels = month.abb[2:9]) +
  
  geom_sf(fill=grey(0.8), size = 0.2, data=land)+
  # 
  # geom_path(data = dplyr::filter(ssm), 
  #           aes(x=lon,y=lat,group=id),  colour = "black",
  #           alpha = 0.6)+
  # geom_point(data = ssm, 
  #            aes(x=lon,y=lat,group=trip, color=date),
  #            alpha = 0.7, shape=19, size = 1)+
  #scale_color_gradientn(colours = rainbow(5), labels=as.Date_origin) +
  coord_sf(xlim = c(-80, -50), ylim=c(-70, -60), expand=c(0,0))+
  labs(x='Longitude',y='Latitude')+
  theme_article(base_size=16) 

