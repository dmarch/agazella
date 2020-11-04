#-----------------------------------------------------------------------
# habitat importance      Calculate habitat importance
#-----------------------------------------------------------------------



# Prepare cluster
cl <- makeCluster(cores)
registerDoParallel(cl)

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

input_data <- paste("data/out/habitat_model/predict", predict_folder, sp_code, mod_code, sep="/")
output_data <- paste("data/out/habitat_model/predict", predict_folder, sp_code, paste("hi",tfreq,sep="-"), sep="/")
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
ncfiles <- list.files(input_data, recursive = TRUE, full.names = TRUE, pattern = "pred.nc")


#----------------------------------------------------------------
# 3. Extract all habitat suitability values across all the period
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
#plot(density(all_vals))


#----------------------------------------------------------------
# 4. Calculate percentiles
#----------------------------------------------------------------

probs <- seq(0, 1, length.out=101)
qtl <- quantile(all_vals, probs = probs)
#plot(probs * 100, qtl, xlab="Percentile (%)", ylab="Habitat suitability")


#----------------------------------------------------------------
# 5. Transform habitat suitability predictions to habitat importance
#----------------------------------------------------------------

# extract values from each raster separatelly
foreach(j=1:length(ncfiles), .packages=c("lubridate", "raster", "stringr", "dplyr", "pals")) %dopar% {
    
  # nc info
  ncfile <- ncfiles[j]
  gpat <- paste0(".*(\\d{8})_", sp_code, "_", mod_code, "_pred.nc")
  strdate <- gsub( gpat, "\\1", ncfile)
  date <- ymd(strdate)
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  
  # open raster
  pred <- raster(ncfile)
  
  # transform to habitat importance
  vals <- raster::values(pred)
  values(pred) <- approx(qtl, probs * 100, vals)$y
  
  # set/create folder
  product_folder <- paste(output_data, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # store file in ncformat
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_hi.nc")
  writeRaster(pred, filename=outfile, format="CDF",overwrite=TRUE)
  
  # export plot
  pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_hi.png")
  png(pngfile, width=1000, height=600, res=100)
  plot(pred, main = paste(sp_name, " -  Habitat importance"), zlim=c(0,100), col = viridis(100))
  plot(land, col="grey80", border="grey60", add=TRUE)
  text(x = -3.5, y = 44, labels = date)
  box()
  dev.off()
}


#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("Habitat importance ready")  
