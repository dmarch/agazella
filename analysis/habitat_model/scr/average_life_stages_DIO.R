#--------------------------------------------------------------------------------------------
# average_life_stages_DIO         Average habitat importance between life stages of same species
#--------------------------------------------------------------------------------------------



#---------------------------------------------------------------
# Set parameters
#---------------------------------------------------------------
sp_code <- sp_code
sp_name <- sp_name
date_start <- date_start 
date_end <- date_end


# Prepare cluster
cl <- makeCluster(cores)
registerDoParallel(cl)

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

input_data <- "data/out/habitat_model/predict"
output_data <- paste("data/out/habitat_model/predict/DIO", paste("hi",tfreq,sep="-"), sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Import data
#---------------------------------------------------------------

# Import landmask (for plot)
data(countriesHigh, package = "rworldxtra", envir = environment())
e <- extent(-8, 17, 33, 46) # raster extent
land <- crop(countriesHigh, e)


#---------------------------------------------------------------
# 3. Average habitat importance on a daily basis
#---------------------------------------------------------------

dates <- seq.Date(date_start, date_end, by="day")  # define sequence


#for(i in 1:length(dates)){
foreach(i=1:length(dates), .packages=c("lubridate", "raster", "stringr", "dplyr", "pals")) %dopar% {
    
  # get date
  date <- dates[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  
  # find netcdfs
  pat <- paste0(".*",  format(date, "%Y%m%d"),"_DIO_", "\\w{3}", "_hi.nc")
  ncfiles <- list.files(input_data, recursive = TRUE, full.names = TRUE, pattern = pat)
  if(length(ncfiles)==0) return(NULL)
  
  # import files
  s <- stack(ncfiles)
  
  # average values
  # use weighted mean. values could be incorporated into species table.
  # weights could be time dependent. check that there no biases due to tagging.
  pred <- mean(s, na.rm=TRUE)
  
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

print("Averaged habitat importance per life stage ready")  