#------------------------------------------------------------------------------------
# oceanmask.R           Create ocean mask to create simulations
#------------------------------------------------------------------------------------
#
# Description:
# Simulations of animal tracks requires setting a map layer to constrain their movement.
# We consider three main constrains of the movement of marine animals:
# 1.- A land mask, to avoid moving into land.
# 2.- A study area, in this case defined by the minimum convex polygon of all tracks.
#
# Returns:
# 1) A minimum convex polygon (shapefile format)
# 2) A raster map (resistance layer) with the following characteristics:
# - CRS is epsg:3031 (simulations seem to perform better using a projected CRS)
# - Resolution is 5 km
# - values are: 0 = ocean, 1 = land


#--------------------------------
# Import data
#--------------------------------

# Import bathymetry
# bathy <- raster("data/gebco/derived_bathy.nc")
bathy_nc <- paste0(output_data, "/terrain/derived_bathy.nc")
bathy <- raster(bathy_nc)

## Import L2 product of simulations (i.e. simulations with environmental data)
indir <- paste0(output_data, "/tracking/", sp_code, "/L2_locations")
loc_files <- list.files(indir, full.names = TRUE, pattern = "L2_locations.csv")
ssm <- readTrack(loc_files)


#--------------------------------
# Minimum convex polygon
#--------------------------------

# Create a minimum convex polygon using all tracks to delimit
ssm$sID <- 1  # create a single ID for all tracks
ssm <- dplyr::select(ssm, sID, lon, lat)
coordinates(ssm) <- ~ lon + lat
proj4string(ssm) <- "+proj=longlat +ellps=WGS84"
cp <- mcp(ssm, percent = 100) # MCP use all the positions

# Extend the MCP with a buffer of 1 degree
cp.buf <- gBuffer(cp, width = mcp_expand)

# Export MCP as shapefile
# cp.df <- data.frame(ID=1:length(cp.buf)) 
# row.names(cp.df) <- "buffer"
# p <- SpatialPolygonsDataFrame(cp.buf, cp.df) 
# writeOGR(p, paste(input_data, "mcp.gpkg", sep="/"), "mcp", driver="GPKG", overwrite_layer=TRUE)


#--------------------------------
# Combine MCP and landmask
#--------------------------------

# Reclassify bathymetry to create ocean mask
# Define resistance values (0 = ocean, 1 = land)
bathy[!is.na(bathy)] <- 0
bathy[is.na(bathy)] <- 1

# Mask with MCP extent
#oceanmask <- mask(bathy, cp.buf)
oceanmask <- bathy

# downsize
oceanmask <- aggregate(oceanmask, fact = 5, fun = median)

# Export resistance
writeRaster(oceanmask, paste0(output_data, "/terrain/oceanmask.nc"), format="CDF", overwrite=TRUE)
