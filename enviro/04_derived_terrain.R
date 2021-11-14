#----------------------------------------------------------------------------------------
# derived_terrain.R         Process bathymetry, slope and distace to shore
#----------------------------------------------------------------------------------------
#
# This script provides the following outputs:
# - Bathymetry cropped by the study area
# - Slope
# - Distance to shore


source("setup.R")

# set output dir
outdir <- paste0(output_data, "/terrain/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import bathymetry
bathync <- paste0(input_data, "/bathymetry/GEBCO_2014_2D.nc")
bathy <- raster(bathync)

# crop to larger region of the stud area to calculate distance to shore
e <- extent(-90, -20, -80, -40)
bathy <- crop(bathy, e)

# set values above 0 as NA
bathy[bathy >= 0] <- NA

# resample to a coarser resolution (0.042 x 0.042 degrees)
bathy_ag <- aggregate(bathy, fact = 20, fun = mean)

# calculate slope
slope <- terrain(bathy_ag, opt=c("slope"), unit='degrees', neighbors=8)

# prepare raster to calculate distance
bathy_d <- bathy_ag
bathy_d[is.na(bathy_d[])] <- 1000 
bathy_d[bathy_d < 1000] <- NA 

# disntace
rdist <- distance(bathy_d)  # calculate distance
rdist <- rdist / 1000  # convert to km
rdist[rdist == 0] <- NA  # set 0 values to NA
sdist <- rdist

# Export derived products
writeRaster(bathy, paste0(outdir, "derived_bathy.nc"), format="CDF", overwrite=TRUE)
writeRaster(bathy_ag, paste0(outdir, "derived_bathy_ag.nc"), format="CDF", overwrite=TRUE)
writeRaster(slope, paste0(outdir, "derived_slope.nc"), format="CDF", overwrite=TRUE)
writeRaster(sdist, paste0(outdir, "derived_sdist.nc"), format="CDF", overwrite=TRUE)


