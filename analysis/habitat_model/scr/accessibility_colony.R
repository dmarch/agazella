#---------------------------------------------------------------------
# accessibility
#---------------------------------------------------------------------


source("setup.R")
source("scr/fun_track_reading.R")  # read multiple tracking data formats
library(scam)
library(pals)


sp_code <- "GAZ"  # species code

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

# input data paths
indir <- paste0(output_data, "/tracking/", sp_code, "/PresAbs")
stack_repo <- paste0(output_data, "/stack_daily")

# output data paths
outdir <- paste(output_data, "habitat-model", sp_code, "accessibility", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

#-----------------------------------------------------------------
# Prepare data
#-----------------------------------------------------------------

# Import observations
obs_file <- paste0(indir, "/", sp_code, "_observations.csv")
data <- read.csv(obs_file)
data$date <- ymd(data$date)


#-----------------------------------------------------------------
# Get presence/absence data and link to distance to colony
#-----------------------------------------------------------------


# import distance to colony
grdfile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = "_enviro.grd")
e <- raster::stack(grdfile[1])
idist <- e$D2COL

## Create an absence map
rabs <- idist
rabs[!is.na(rabs)] <- 0

## Create a presence map with both observed and simulated trips
rpres <- rasterize(cbind(data$lon, data$lat), idist)
rpres <- rpres/rpres

## Create presence/absence
rpa <- sum(rpres, rabs, na.rm=TRUE)
rpa <- raster::mask(rpa, rabs)
names(rpa) <- "OCC"

## Prepare data.frame
s <- stack(rpa, idist)  # combine
dist <- data.frame(na.omit(values(s)))



#-----------------------------------------------------------------
# Fit binomial models
#-----------------------------------------------------------------

# if the dataset is too large generate a random subsample without replacement
# stratification by prevalence and date
if(nrow(dist) > 40000){
  dist <- sample_n(dist, size = 40000, replace = FALSE)
} 


# Fitted binomial models with a smooth, monotonic decreasing constraint (see Hindell 2020)
# Check: https://github.com/SCAR/RAATD/blob/master/Code/runAvailabilityModels_04.R
# For agazella: https://github.com/SCAR/RAATD/blob/master/Code/runAvailabilityModels_CRAS%26WESE.R
scamMod <- scam(formula = OCC ~ s(D2COL, bs="mpd"),  # Monotone decreasing P-splines
                family = binomial,
                data = dist)

# save
saveRDS(scamMod, paste0(outdir, "/d2col_accessScam.RDS"))


# plot
dfpred <- data.frame(D2COL = seq(minValue(idist), maxValue(idist), 1000))
dfpred$pre <- predict.scam(scamMod, newdata = dfpred, type="response")
plot(dfpred$D2COL, dfpred$pre, type="l")

#-----------------------------------------------------------------
# Predict model
#-----------------------------------------------------------------

## Predict model to each colony
accessibility <- predict(idist, scamMod, type="response")



#-----------------------------------------------------------------
# Export
#-----------------------------------------------------------------

# Export raster
writeRaster(accessibility, paste0(output_data, "/", sp_code, "_access.grd"), bandorder='BIL', overwrite=TRUE)
