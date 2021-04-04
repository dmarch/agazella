#---------------------------------------------------------------------------------------------------
# predict_brt          Predict BRT
#---------------------------------------------------------------------------------------------------

mod_code <- "brt"
cores <- 20
date_start <- as.Date("2019-04-11")  # change to 2012 in final version


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste(output_data, "habitat-model", sp_code, mod_code, sep="/")
outdir <- paste(output_data, "habitat-model", sp_code, mod_code, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

## Import landmask
world <- ne_countries(scale = "medium", returnclass = "sp")
land <- crop(world, extent(-90, -20, -80, -50))

# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/")
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)

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
  stack_list <- list()
  #foreach(j=1:length(models)) %do% {
  #foreach(j=1:2, .packages=c("raster", "dismo", "gbm")) %dop% {
  for(j in 1:length(models)){  
    pred <- raster::predict(model = models[[j]], object = s, n.trees=models[[j]]$gbm.call$best.trees, type="response")
    stack_list[[j]] <- pred
  }
  
  # create stack from list
  pred_stack <- stack(stack_list)
  
  # Average predictions
  pred <- mean(pred_stack)
  pred_sd <- calc(pred_stack, sd)
  
  # set/create folder
  product_folder <- paste(outdir, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # store file in ncformat
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred.tif")
  writeRaster(pred, filename=outfile, overwrite=TRUE)

  # store file in ncformat
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred_sd.tif")
  writeRaster(pred_sd, filename=outfile, overwrite=TRUE)
  
  # export plot
  pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred.png")
  png(pngfile, width=560, height=600, res=100)
  plot(pred, main = paste(sp_name, "   Model:", mod_code, "\n", date), zlim=c(0,1), col = viridis(100))
  plot(land, col="grey80", border="grey60", add=TRUE)
  text(x = -3.5, y = 44, labels = date)
  box()
  dev.off()
  
  # export plot
  pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred_sd.png")
  png(pngfile, width=560, height=600, res=100)
  plot(pred_sd, main = paste(sp_name, "   Model:", mod_code, "\n", date), zlim=c(0,1), col = viridis(100))
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

