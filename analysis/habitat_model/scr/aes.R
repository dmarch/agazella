#-----------------------------------------------------------------------
# aes
#-----------------------------------------------------------------------

#library(sf)
library(smoothr)
library(lwgeom)

# Prepare cluster
cl <- makeCluster(cores)
registerDoParallel(cl)


aes_prob <- 0.9  # quantile probability to define the AES
min_pol_size <- 1000  # minimum size to define a spatial coherent area, in km2  (higher number result in less polygons) 



#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

input_data <- "data/out/habitat_model/predict/all/hi"
output_data <- "data/out/habitat_model/predict/all/aes"
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Import data
#---------------------------------------------------------------

# Import landmask (for plot)
data(countriesHigh, package = "rworldxtra", envir = environment())
e <- extent(-8, 17, 33, 46) # raster extent
land <- crop(countriesHigh, e)

# import prediction
# list all prediction files
ncfiles <- list.files(input_data, recursive = TRUE, full.names = TRUE, pattern = "hi.nc")



#----------------------------------------------------------------
# 3. Extract all habitat importance values across all the period
#----------------------------------------------------------------

# extract values from each raster separatelly
val_list <- (foreach(j=1:length(ncfiles), .packages=c("lubridate", "raster", "stringr", "dplyr", "pals")) %dopar% {
  # extract values from raster
  r <- raster(ncfiles[j])
  vals <- raster::values(r)
  # remove NAs
  vals <- vals[!is.na(vals)]
  # remove zeros
  #vals <- vals[vals != 0]
  vals
})

# combine all values
all_vals <- unlist(val_list)
min_val <- min(all_vals)
max_val <- max(all_vals)
#plot(density(all_vals))



#----------------------------------------------------------------
# 4. Calculate percentiles
#----------------------------------------------------------------

aes_qtl <- quantile(all_vals, probs = aes_prob)


#----------------------------------------------------------------
# 5. Transform habitat suitability predictions to habitat importance
#----------------------------------------------------------------

# extract values from each raster separatelly
foreach(j=1:length(ncfiles), .packages=c("lubridate", "raster", "stringr", "dplyr", "pals", "smoothr", "sf", "lwgeom", "rgdal")) %dopar% {
  
  # nc info
  ncfile <- ncfiles[j]
  gpat <- paste0(".*(\\d{8})_ALL_hi.nc")
  strdate <- gsub( gpat, "\\1", ncfile)
  date <- ymd(strdate)
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  
  # set/create folder
  product_folder <- paste(output_data, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  
  # open raster
  pred <- raster(ncfile)
  
  #reclass into AES
  m <- c(0,aes_qtl,NA, aes_qtl,100,1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  aes <- reclassify(pred, rclmat)
  
  # store file in ncformat
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_ALL_aes.nc")
  writeRaster(aes, filename=outfile, format="CDF",overwrite=TRUE)
  
  ## convert raster into polygon
  r_poly <- rasterToPolygons(aes, dissolve=TRUE)
  if(is.null(r_polu)) return(NULL)
  
  # Drop small, isolated cells
  area_thresh <- units::set_units(min_pol_size, km^2)
  r_poly_dropped <- drop_crumbs(r_poly, area_thresh)
  
  if(!is.null(r_poly_dropped)){
    # Fill small holes
    r_poly_filled <- fill_holes(r_poly_dropped, area_thresh)
    
    # And smooth the edges
    r_poly_smooth <- smoothr::smooth(r_poly_filled, method = "ksmooth", smoothness = 5)
    
    # save polygon
    outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_ALL_aes.gpkg")
    writeOGR(r_poly_smooth, dsn = outfile, layer="aes", driver="GPKG", overwrite_layer = TRUE)

    # export plot
    pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_ALL_aes.png")
    png(pngfile, width=1000, height=600, res=100)
    plot(pred, main = "Overall habitat importance (with AES)", zlim=c(min_val, max_val), col = viridis(100))
    plot(land, col="grey80", border="grey60", add=TRUE)
    plot(r_poly_smooth, border="black", add=TRUE)
    text(x = -3.5, y = 44, labels = date)
    box()
    dev.off()
  } else {
    
    # export plot
    pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_ALL_aes.png")
    png(pngfile, width=1000, height=600, res=100)
    plot(pred, main = "Overall habitat importance (with AES)", zlim=c(min_val, max_val), col = viridis(100))
    plot(land, col="grey80", border="grey60", add=TRUE)
    text(x = -3.5, y = 44, labels = date)
    box()
    dev.off()
  }
}







#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("AES ready")  