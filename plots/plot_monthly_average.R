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
boot <- T

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
if(boot == F) indir <- paste(output_data, "habitat-model", sp_code, mod_code, "predict", sep="/")
if(boot == T) indir <- paste(output_data, "habitat-model", sp_code, mod_code, "predict_boost", sep="/")
outdir <- paste(output_data, "habitat-model", sp_code, mod_code, "monthly-plots", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



#---------------------------------------------------------------
# 2. Prepare data
#---------------------------------------------------------------

# list all files per month
predict_files <- tibble(
  file = list.files(indir, pattern = 'pred.tif', recursive = T, full.names = T),
  date = ymd(str_extract(file, pattern = '[[:digit:]]{4}[[:digit:]]{2}[[:digit:]]{2}')),
  month = month(date))

# landmask
world <- ne_countries(scale = "medium", returnclass = "sf")
e <- extent(-90, -20, -80, -50)
box = c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4])
land <- st_crop(world, box)


#---------------------------------------------------------------
# 3. Monthly averages
#---------------------------------------------------------------

m <- unique(predict_files$month)

mavg_all <- stack()

for(i in m){
  
  # select files to import
  files <- predict_files %>% filter(month == i)
  
  # raster stack
  s <- stack(files$file)
  
  # average
  mavg <- mean(s, na.rm=TRUE)
  mavg_all <- stack(mavg_all, mavg)
}

# names layers
names(mavg_all) <- month.abb[m]



# import raster
rasdf <- as.data.frame(mavg_all,xy=TRUE)%>%drop_na()
rasdf_m <- setDT(melt(rasdf, id.vars = c("x","y")))


# plot
p <- ggplot()+
  geom_raster(aes(x=x,y=y,fill=value),data=rasdf_m)+
  geom_sf(fill=grey(0.8), size = 0.2, data=land)+
  scale_fill_viridis_c('Habitat suitability', limits=c(0,1))+
  facet_wrap(~variable, ncol = 4)+
  coord_sf(expand=c(0,0))+
  scale_x_continuous(breaks = seq(-80, -20, 20)) +
  labs(x='',y='')+
  #annotate(geom="label", x=-80, y=-77, label=idate, fill="white", size = 5) +
  theme_article(base_size=16) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 14),
        legend.key.height=grid::unit(0.6,"cm"),
        legend.key.width=grid::unit(1.6,"cm"),
        legend.text = element_text(size = 12))+
   # legend title
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               frame.colour = "black",
                               ticks = TRUE))

# Export plot
if(boot == F) p_png <- paste0(outdir, "/monthly_average.png")
if(boot == T) p_png <- paste0(outdir, "/monthly_average_boot.png")
ggsave(p_png, p, width=30, height=20, units="cm", dpi=300)



#### Calculate monthly sea ice edge
# Monthly sea ice area is derived from the monthly mean of sea ice cover

# list all stackfiles per month
enviro_files <- tibble(
  file = list.files(stack_repo, pattern = '_enviro.grd', recursive = T, full.names = T),
  date = ymd(str_extract(file, pattern = '[[:digit:]]{4}[[:digit:]]{2}[[:digit:]]{2}')),
  month = month(date))



m <- unique(enviro_files$month)

mavg_all <- stack()

for(i in m){
  
  # select files to import
  files <- enviro_files %>% filter(month == i)
  
  # read all SIC rasters (band = 11)
  s <- stack()
  for(j in files$file){
    r <- raster(j, band = 11)
    s <- stack(s, r)
  }
  
  # average
  mavg <- mean(s, na.rm=TRUE)
  mavg_all <- stack(mavg_all, mavg)
}

# names layers
names(mavg_all) <- month.abb[m]

# Calculate edge
cnt <- rasterToContour(mavg_all$Feb, levels = 0.15)

