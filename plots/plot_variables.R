#--------------------------------------------------
# plot_variables
#--------------------------------------------------


source("setup.R")


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste(output_data, "stack_daily", sep="/")
outdir <- paste(output_data, "stack_daily", "plots", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



#---------------------------------------------------------------
# 2. Prepare data
#---------------------------------------------------------------

# set list of predictors
vars <- c("BAT", "SLP", "SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "MLD", "EDGE") 

# list all files per month
stack_files <- tibble(
  file = list.files(indir, pattern = '.grd', recursive = T, full.names = T),
  date = ymd(str_extract(file, pattern = '[[:digit:]]{4}[[:digit:]]{2}[[:digit:]]{2}')))

# landmask
world <- ne_countries(scale = "medium", returnclass = "sf")
e <- extent(-90, -20, -80, -50)
box = c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4])
land <- st_crop(world, box)


#---------------------------------------------------------------
# 3. Plot
#---------------------------------------------------------------

s <- stack(stack_files$file[85])  #15 May


# import raster
rasdf <- as.data.frame(s, xy=TRUE) %>% drop_na()
rasdf_m <- setDT(melt(rasdf, id.vars = c("x","y")))

# select variables
rasdf_m <- rasdf_m %>% filter(variable %in% vars)

# plot
# trick part here is that we need individual scales
# we need to separete ggplot objects and then combine


for(i in vars){
  
  p <- ggplot()+
    geom_raster(aes(x=x,y=y,fill=value),data=filter(rasdf_m, variable == i))+
    geom_sf(fill=grey(0.8), size = 0.2, data=land)+
    scale_fill_viridis_c(name=i)+
    coord_sf(expand=c(0,0))+
    scale_x_continuous(breaks = seq(-80, -20, 20)) +
    labs(x='', y='')+
    theme_article(base_size=16) +
    theme(legend.position = "bottom",
          legend.key.height=grid::unit(0.6,"cm"),
          legend.key.width=grid::unit(1.6,"cm"),
          legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
          axis.title.x = element_text(margin = ggplot2::margin(t = 0, r = 0, b = -2, l = 0))) +
    guides(fill = guide_colorbar(title.position = "top",
                                 title.hjust = 0.5,
                                 frame.colour = "black",
                                 ticks = TRUE))
  
  # Export plot
  p_png <- paste0(outdir, sprintf("/%s.png", i))
  ggsave(p_png, p, width=15, height=15, units="cm", dpi=300)
  
}


