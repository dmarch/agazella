#--------------------------------------------------
# plot_monthly_averages
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
outdir <- paste(output_data, "habitat-model", sp_code, mod_code, "animation", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



# import animal tracks
ssm_dir<- paste0(output_data, "/tracking/", sp_code, "/L2_locations")
ssm_files <- list.files(ssm_dir, full.names = T, pattern = "L2_locations.csv")
ssm <- readTrack(ssm_files)

### create map template in ggplot
all_maps <- list.files(indir, recursive = T, full.names = T, pattern = ".tif")

# landmask
world <- ne_countries(scale = "medium", returnclass = "sf")
e <- extent(-90, -20, -80, -50)
box = c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4])
land <- st_crop(world, box)

# Prepare cluster
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i=1:length(all_maps), .packages=c("tidyr", "lubridate", "raster", "stringr", "dplyr", "ggplot2", "egg")) %dopar% {

  # file info
  ifile <- all_maps[i]
  idate <- ymd(str_extract(file, pattern = '[[:digit:]]{8}'))
  
  # import raster
  r <- raster(ifile)
  rasdf <- as.data.frame(r,xy=TRUE)%>%drop_na()
  names(rasdf)[3] <- "layer"

  # plot
  p <- ggplot()+
    #geom_raster(aes(x=x,y=y,fill=layer),data=rasdf)+
    geom_sf(fill=grey(0.8), size = 0.2, data=land)+
    #scale_fill_viridis_c('Habitat suitability', limits=c(0,1))+
    
    
    # lines and points
    geom_path(data = dplyr::filter(ssm, date >= idate-10, date <= idate), 
              aes(x=lon,y=lat,group=id),  color = "red",
              alpha = 0.6)+
    geom_point(data = dplyr::filter(ssm, date == idate), 
               aes(x=lon,y=lat), color = "red",
               alpha = 0.9, shape=21, size = 5)+
    
    coord_sf(expand=c(0,0))+
    labs(x='Longitude',y='Latitude')+
    annotate(geom="label", x=-80, y=-77, label=date, fill="white", size = 5) +
    theme_article(base_size=16) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(size = 15),
          legend.key.height=grid::unit(0.6,"cm"),
          legend.key.width=grid::unit(2,"cm"),
          legend.text = element_text(size = 13))+#,
    #axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    #axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    # legend title
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, ticks = TRUE))
  
  
  # Export plot
  p_png = paste0(outdir, "/", sprintf("%s_brt.png", format(date, "%Y%m%d")))
  ggsave(p_png, p, width=18, height=20, units="cm", dpi=300)
}

stopCluster(cl)


library(gifski)

files <- list.files(outdir, full.names = T, pattern = ".png")

# animation
gifski(files, "agazella_brt.gif", width = 1800, height = 2000, loop = FALSE, delay = 0.5)
