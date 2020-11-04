
source("scr/fun_enviro.R")



#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

# jrc folders
if(sp_code == "FWH") jrc_folder <- "EMIS_Fin_whale"
if(sp_code == "BFT_ADL") jrc_folder <- "EMIS_BFT500_FH"
if(sp_code == "BFT_JUV") jrc_folder <- "EMIS_BFT25_FH"
if(sp_code == "BFT_SPW") jrc_folder <- "EMIS_BFT500_SH"


input_data <- paste0("data/raw/models/", jrc_folder)
output_data <- paste("data/out/habitat_model/predict", predict_folder, sp_code, mod_code, sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Import data
#---------------------------------------------------------------

# Import oceanmask
ocean <- readOGR("data/raw/ext/oceanmask","WestMed_area")
ocean <- spTransform(ocean, crs_proj)

# list netcdf
ncfiles <- list.files(input_data, full.names = TRUE, pattern = ".nc")

# Import landmask (for plot)
data(countriesHigh, package = "rworldxtra", envir = environment())
e <- extent(-8, 17, 33, 46) # raster extent
land <- crop(countriesHigh, e)

#---------------------------------------------------------------
# 3. Create a common mask (See enviro_stack_daily)
#---------------------------------------------------------------

# Set raster resolution
res <- 0.05

# create base raster
r <- raster(extent(ocean), res = res, crs = crs(ocean))

# rasterize ocean mask
m <- rasterize(as(ocean,"SpatialPolygons"), r)


#---------------------------------------------------------------
# 4. Process files
#---------------------------------------------------------------

for (i in 1:length(ncfiles)){
  
  # get year and month from global attributes
  ncfile <- ncfiles[i]
  nc <- nc_open(ncfile)
  YYYY <- ncatt_get(nc,0,"Year")$value
  MM  <- ncatt_get(nc,0,"Month")$value
  
  # check if year alligns with our period of study
  if(!as.numeric(YYYY) %in% years) next
  
  # import raster
  rmodel <- raster(ncfile)
  
  # resample
  pred <- prepareGrid(rmodel, m, method="bilinear", name=sp_code)
  
  # set/create folder
  product_folder <- paste(output_data, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # store file in ncformat
  outfile <- paste0(product_folder, "/", paste0(YYYY,MM,"01"),"_", sp_code, "_", mod_code, "_pred.nc")
  writeRaster(pred, filename=outfile, format="CDF",overwrite=TRUE)
  
  # export plot
  pngfile <- paste0(product_folder, "/", paste0(YYYY,MM,"01"),"_", sp_code, "_", mod_code, "_pred.png")
  png(pngfile, width=1000, height=600, res=100)
  plot(pred, main = paste(sp_name, "   Model:", mod_code), zlim=c(0,100), col = viridis(100))
  plot(land, col="grey80", border="grey60", add=TRUE)
  text(x = -3.5, y = 44, labels = paste(YYYY,MM,"01", sep="-"))
  box()
  dev.off()
  
}

