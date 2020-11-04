#---------------------------------------------------------------------------------------------------
# predict_sdm             Predict Species distribution model
#---------------------------------------------------------------------------------------------------

library(pals)

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
input_data <- paste0("results/habitat_model/", sp_code)
output_data <- paste("data/out/habitat_model/predict", predict_folder, sp_code, mod_code, sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

stack_path <- "data/out/environment/stack_daily"



## Import landmask
data(countriesHigh, package = "rworldxtra", envir = environment())
e <- extent(-90, -20, -80, -40)
land <- crop(countriesHigh, e)



# Import model
mod <- readRDS(paste0(input_data, "/", sp_code, "_", mod_code, ".rds"))

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
  grdfile <- list.files(stack_path, recursive = TRUE, full.names = TRUE, pattern = pat)
  
  # Import environmental stack
  s <- stack(grdfile)
  
  # Transform variables
  s$CHL <- log1p(s$CHL)
  s$EKE <- log1p(s$EKE)
  
  # Model prediction
  if(mod_code == "me") pred <- raster::predict(model = mod, object = s)
  if(mod_code == "brt") pred <- raster::predict(model = mod, object = s, n.trees=mod$gbm.call$best.trees, type="response")

  # set/create folder
  product_folder <- paste(output_data, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # store file in ncformat
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred.nc")
  writeRaster(pred, filename=outfile, format="CDF",overwrite=TRUE)

  # export plot
  pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred.png")
  png(pngfile, width=1000, height=600, res=100)
  plot(pred, main = paste(sp_name, "   Model:", mod_code), zlim=c(0,1), col = viridis(100))
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

