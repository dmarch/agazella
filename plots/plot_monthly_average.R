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
library(scico)

mod_code <- "brt"
sp_code <- "GAZ"
boot <- T

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
if(boot == F) indir <- paste(output_data, "habitat-model-v2", sp_code, mod_code, "predict", sep="/")
if(boot == T) indir <- paste(output_data, "habitat-model-v2", sp_code, mod_code, "predict_boost", sep="/")
outdir <- paste(output_data, "habitat-model-v2", sp_code, mod_code, "monthly-plots", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



#---------------------------------------------------------------
# 2. Prepare data
#---------------------------------------------------------------



# landmask
world <- ne_countries(scale = "medium", returnclass = "sf")
e <- extent(-90, -20, -80, -50)
box = c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4])
land <- st_crop(world, box)


#---------------------------------------------------------------
# 3. Monthly averages (median)
#---------------------------------------------------------------

# list all files per month
predict_files <- tibble(
  file = list.files(indir, pattern = 'pred.tif', recursive = T, full.names = T),
  date = ymd(str_extract(file, pattern = '[[:digit:]]{4}[[:digit:]]{2}[[:digit:]]{2}')),
  month = month(date))

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

# set xy limits
yl <- c(-75, -50)

# set scale range
range(rasdf_m$value)
zl <- c(0, 1)

# plot
p <- ggplot()+
  geom_raster(aes(x=x,y=y,fill=value),data=rasdf_m)+
  geom_sf(fill=grey(0.8), size = 0.2, data=land)+
  scale_fill_viridis_c('Habitat suitability (median)', limits=zl)+
  facet_wrap(~variable, ncol = 4)+
  coord_sf(ylim = yl, expand=c(0,0))+
  scale_x_continuous(breaks = seq(-75, -35, 20)) +
  labs(x='',y='')+
  theme_article(base_size=16) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 14, angle = 90),
        legend.key.height=grid::unit(2,"cm"),
        legend.key.width=grid::unit(0.4,"cm"),
        legend.text = element_text(size = 12),
        legend.margin = ggplot2::margin(0,0,0,0),
        legend.box.margin = ggplot2::margin(0,0,0,0),
        plot.margin = unit(c(10,20,10,10), "points")) +
   # legend title
  guides(fill = guide_colorbar(title.position = "right",
                               title.hjust = 0.5,
                               frame.colour = "black",
                               ticks = TRUE))

# Export plot
if(boot == F) p_png <- paste0(outdir, "/monthly_average.png")
if(boot == T) p_png <- paste0(outdir, "/monthly_average_boot.png")
ggsave(p_png, p, width=30, height=14, units="cm", dpi=300)



#---------------------------------------------------------------
# 3. Monthly averages (95% CI)
#---------------------------------------------------------------


# list all files per month
predict_files <- tibble(
  file = list.files(indir, pattern = 'pred_cir.tif', recursive = T, full.names = T),
  date = ymd(str_extract(file, pattern = '[[:digit:]]{4}[[:digit:]]{2}[[:digit:]]{2}')),
  month = month(date))

m <- unique(predict_files$month)

mCI_all <- stack()

for(i in m){
  
  # select files to import
  files <- predict_files %>% filter(month == i)
  
  # raster stack
  s <- stack(files$file)
  
  # average
  mavg <- mean(s, na.rm=TRUE)
  mCI_all <- stack(mCI_all, mavg)
}

# names layers
names(mCI_all) <- month.abb[m]



# import raster
rasdf <- as.data.frame(mCI_all,xy=TRUE)%>%drop_na()
rasdf_m <- setDT(melt(rasdf, id.vars = c("x","y")))

# set xy limits
yl <- c(-75, -50)

# set scale range
zl <- c(0, 0.5)

# plot
p <- ggplot()+
  geom_raster(aes(x=x,y=y,fill=value),data=rasdf_m)+
  geom_sf(fill=grey(0.8), size = 0.2, data=land)+
  scale_fill_scico(palette = "lajolla", direction = 1, limits = zl,                
                   name='Habitat suitability (95% CI range)') +
  facet_wrap(~variable, ncol = 4)+
  coord_sf(ylim = yl, expand=c(0,0))+
  scale_x_continuous(breaks = seq(-75, -35, 20)) +
  labs(x='',y='')+
  #annotate(geom="label", x=-80, y=-77, label=idate, fill="white", size = 5) +
  theme_article(base_size=16) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 14, angle = 90),
        legend.key.height=grid::unit(2,"cm"),
        legend.key.width=grid::unit(0.4,"cm"),
        legend.text = element_text(size = 12),
        legend.margin = ggplot2::margin(0,0,0,0),
        legend.box.margin = ggplot2::margin(0,0,0,0),
        plot.margin = unit(c(10,20,10,10), "points")) +
  # legend title
  guides(fill = guide_colorbar(title.position = "right",
                               title.hjust = 0.5,
                               frame.colour = "black",
                               ticks = TRUE))

# Export plot
if(boot == T) p_png <- paste0(outdir, "/monthly_averageCIR_boot.png")
ggsave(p_png, p, width=30, height=14, units="cm", dpi=300)



