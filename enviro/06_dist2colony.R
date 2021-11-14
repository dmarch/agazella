#-------------------------------------------------------------------------------
# 07_dist2colony        Map of distance to deployment colony
#-------------------------------------------------------------------------------


source("setup.R")


# set output dir
outdir <- paste0(output_data, "/terrain/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


# set coordinates to the deployment colony
colony <- list(deception = c(-60.54, -63.00))

# path to environmental static data
static_data <- paste0(output_data, "/terrain/")

# import static maps
bathy <- raster(paste0(static_data, "/derived_bathy_ag.nc"))  # bathymetry
bathy <- bathy+0

# create ocean mask using the bathymetry
mask <- bathy/bathy

# create surface
tr1 <- transition(mask, transitionFunction=mean, directions=16)
tr1C <- geoCorrection(tr1)

# calculate distance from deployment colony
A <- accCost(tr1C, colony[[1]])
A[is.infinite(A)] <- NA

# Export derived products
writeRaster(A, paste0(outdir, "d2col.nc"), format="CDF", overwrite=TRUE)
