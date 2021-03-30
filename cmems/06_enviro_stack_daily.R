#-------------------------------------------------------------------------------------------
# 05_enviro_stack_daily.R        Create environmental daily stacks of environmental data
#-------------------------------------------------------------------------------------------
# This script generates a daily multiband raster to then make model predictions.

#source("conf/config.R")
source("setup.R")
source("scr/fun_enviro.R")  # function to process environmental data



### TODO:
# Round values of each variable



#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

# set number of cores
cores <- 10

# Set raster resolution and extent
res <- 0.1
e <- extent(-90, -20, -80, -50)

# Set period
date_start <- as.Date("2019-02-01")
date_end <- as.Date("2019-02-19")

#date_end <- as.Date("2019-09-15")

# dynamic variables to extract. same names as catalog
env_dyn_vars <- c("SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "SIT", "MLD", "EDGE") 

# path to environmental static data
static_data <- paste0(output_data, "/terrain/")

# path to output
outdir <- paste0(output_data, "/stack_daily/")#"data/out/environment/stack_daily"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



#---------------------------------------------------------------
# 1. Create oceanmask
#---------------------------------------------------------------

# Import oceanmask
# ocean <- readOGR("data/raw/ext/oceanmask","WestMed_area")
# ocean <- spTransform(ocean, crs_proj)



# create base raster
m <- raster(e, res = res, crs = crs("+proj=longlat +datum=WGS84"))
m[] <- 1

# rasterize ocean mask
# m <- rasterize(as(ocean,"SpatialPolygons"), r)
# 
# # export ocean mask for further analysis
# writeRaster(m, "data/out/environment/static/oceanmask.nc", format="CDF",overwrite=TRUE)

#---------------------------------------------------------------
# 2. Import environmental data
#---------------------------------------------------------------

# import static maps
bathy <- raster(paste0(static_data, "/derived_bathy_ag.nc"))  # bathymetry
bathy <- bathy+0
slope <- raster(paste0(static_data, "/derived_slope.nc"))  # slope
slope <- slope+0
sdist <- raster(paste0(static_data, "/derived_sdist.nc"))  # distance to shore
sdist <- sdist+0


# import catalogue with oceanographic products
provider_paths <- list(CMEMS = paste0(input_data, "/cmems"),#"data/cmems",
                       MOVEMED = paste0(input_data, "/cmems"))#"data/cmems")
catalog <- import_catalog("cmems/agazella_catalog.csv", provider_paths)


#-------------------------------------------------------
# 3. Prepare static variables
#-------------------------------------------------------

# create stack with static variables
bat <- prepareGrid(bathy, m, method="bilinear", name="BAT")
slp <- prepareGrid(slope, m, method="bilinear", name="SLP")
sdist <- prepareGrid(sdist, m, method="bilinear", name="SDIST")
stack_static <- stack(bat, slp, sdist)



#-------------------------------------------------------
# 4. Prepare dynamic variables
#-------------------------------------------------------

# Prepare cluster
cl <- makeCluster(cores)
registerDoParallel(cl)

# Create dates
dates <- seq.Date(date_start, date_end, by="day")

# Process data
foreach(i=1:length(dates), .packages=c("lubridate", "raster", "stringr", "dplyr")) %dopar% {

  # set day i
  date <- dates[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  print(date)
  
  # creaty empty stack
  stack_dynamic <- stack()
  
  # loop for each dynamic variable
  for (j in 1:length(env_dyn_vars)){
    
    jvar <- prepareGridCatalogue(var = env_dyn_vars[j], catalog = catalog, name = env_dyn_vars[j], date = date, m = m, method = "bilinear")
    stack_dynamic <- stack(stack_dynamic, jvar)
  }
  
  # modify ice variables to add zero values
  stack_dynamic$SIC[is.na(stack_dynamic$SIC)] <- 0
  stack_dynamic$SIC <- raster::mask(stack_dynamic$SIC, bat)
  
  stack_dynamic$SIT[is.na(stack_dynamic$SIT)] <- 0
  stack_dynamic$SIT <- raster::mask(stack_dynamic$SIT, bat)
  
  # combine with static stack
  s <- stack(stack_static, stack_dynamic)
  s <- setZ(s, rep(date, nlayers(s)))
  
  # set/create folder
  product_folder <- paste(outdir, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # store file in GRD format
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_enviro.grd")
  writeRaster(s, outfile, bandorder='BIL', overwrite=TRUE)
}



#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("Daily stack ready")  

