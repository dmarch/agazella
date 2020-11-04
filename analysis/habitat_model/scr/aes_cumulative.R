#------------------------------------------------------------------------
# aes_cumulative.R        Cumulative AES
#------------------------------------------------------------------------
# Combine all AES maps


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

input_data <- "data/out/habitat_model/predict/all/aes"

# list all prediction files
ncfiles <- list.files(input_data, recursive = TRUE, full.names = TRUE, pattern = "aes.nc")


# Import landmask (for plot)
data(countriesHigh, package = "rworldxtra", envir = environment())
e <- extent(-8, 17, 33, 46) # raster extent
land <- crop(countriesHigh, e)

# Import oceanmask
oceanmask <- raster("data/out/environment/static/oceanmask.nc")

#---------------------------------------------------------------
# 2. Combine all files
#---------------------------------------------------------------

# import nc
aes <- stack(ncfiles)

# calculate percentage of AES selected across all period
cum_aes <- (sum(aes, na.rm=TRUE)/nlayers(aes)) * 100

# mask
cum_aes <- mask(cum_aes, oceanmask)


# export plot
pngfile <- paste0(input_data, "/", "persistence.png")
png(pngfile, width=1000, height=600, res=100)
plot(cum_aes, main = "AES (annual persistence)", zlim=c(0, ceiling(maxValue(cum_aes))), col = viridis(100))
plot(land, col="grey80", border="grey60", add=TRUE)
box()
dev.off()
