#-----------------------------------------------------------------------
# predict_accesss       Predict with accessibility
#-----------------------------------------------------------------------
# Use unweighted. Discuss later with UB team which model use.






#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

input_data <- paste("data/out/habitat_model/predict", predict_folder, sp_code, mod_code, sep="/")
output_data <- paste("data/out/habitat_model/predict", predict_folder, sp_code, paste0(mod_code, "_acc"), sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Import data
#---------------------------------------------------------------

# Import accessibility
access <- stack(paste0("results/habitat_model/", sp_code, "/", sp_code, "_access.grd"))

# select accessibility model
acc <- subset(access, which(names(access) == mod_acc))

## Import landmask
data(countriesHigh, package = "rworldxtra", envir = environment())
e <- extent(-8, 17, 33, 46) # raster extent
land <- crop(countriesHigh, e)



# Prepare cluster
cl <- makeCluster(cores)
registerDoParallel(cl)


# import prediction
# list all prediction files
ncfiles <- list.files(input_data, recursive = TRUE, full.names = TRUE, pattern = "pred.nc")

#for (j in 1:length(ncfiles)){
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
  
  # calculate product with accessibility
  pred_acc <- pred * acc

  # set/create folder
  product_folder <- paste(output_data, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  
  # store file in ncformat
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_acc_pred.nc")
  writeRaster(pred_acc, filename=outfile, format="CDF", overwrite=TRUE)
  
  # export plot
  pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_acc_pred.png")
  png(pngfile, width=1000, height=600, res=100)
  plot(pred_acc, main = paste(sp_name, "   Model:", mod_code, "(acc)"), zlim=c(0,1), col = viridis(100))
  plot(land, col="grey80", border="grey60", add=TRUE)
  text(x = -3.5, y = 44, labels = date)
  box()
  dev.off()
}

#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("Prediction with accessibility ready")  
