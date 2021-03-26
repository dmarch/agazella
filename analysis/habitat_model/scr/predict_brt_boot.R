#---------------------------------------------------------------------------------------------------
# predict_brt          Predict BRT
#---------------------------------------------------------------------------------------------------



#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste(output_data, "habitat-model", sp_code, mod_code, sep="/")
outdir <- paste(output_data, "habitat-model", sp_code, mod_code, "predict", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

## Import landmask
world <- ne_countries(scale = "medium", returnclass = "sp")
land <- crop(world, extent(-90, -20, -80, -50))

# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/")
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)

# import model
ibrt <- readRDS(boots_files[i])


# Prepare cluster
cl <- makeCluster(cores)
registerDoParallel(cl)

# Create dates
dates <- seq.Date(date_start, date_end, by="day")  # define sequence

foreach(i=1:length(dates), .packages=c("lubridate", "raster", "stringr", "dplyr", "pals", "dismo", "gbm")) %dopar% {

  # Get time information
  date <- dates[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  
  # Locate file
  pat <- paste0(format(date, "%Y%m%d"), "_enviro.grd")
  grdfile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
  
  # Import environmental stack
  s <- stack(grdfile)
  s <- s+0
  
  # Transform variables
  s$CHL <- log1p(s$CHL)
  s$EKE <- log1p(s$EKE)
  
  # Model prediction
  pred_stack <- stack()
  foreach(j=1:length(models)) %do% {
    pred <- raster::predict(model = models[[j]], object = s, n.trees=models[[j]]$gbm.call$best.trees, type="response")
    pred_stack <- stack(pred_stack, pred)
  }
  
  # Average predictions
  pred <- mean(pred_stack)
  pred <- calc(pred_stack, sd)
  
  # set/create folder
  product_folder <- paste(outdir, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # store file in ncformat
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred.tif")
  writeRaster(pred, filename=outfile, overwrite=TRUE)

  # export plot
  pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred.png")
  png(pngfile, width=560, height=600, res=100)
  plot(pred, main = paste(sp_name, "   Model:", mod_code, "\n", date), zlim=c(0,1), col = viridis(100))
  plot(land, col="grey80", border="grey60", add=TRUE)
  text(x = -3.5, y = 44, labels = date)
  box()
  dev.off()
}

#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("Prediction ready")  

