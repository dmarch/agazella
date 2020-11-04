#---------------------------------------------------------------------
# accessibility
#---------------------------------------------------------------------



source("scr/fun_track_reading.R")  # read multiple tracking data formats
library(scam)
library(pals)

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
input_data <- "data/out/habitat_model/observations/"
output_data <- paste0("results/habitat_model/", sp_code)
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

#-----------------------------------------------------------------
# Prepare data
#-----------------------------------------------------------------

# Import observations
obs_file <- paste0(input_data, sp_code, "_observations.csv")
data <- read.csv(obs_file)

# Select colonies
cols <- unique(data$colony)

# import colony locations and sizes
colonies <- read.csv("data/raw/tracking/UB/CALDIO_poblacions.csv")
colonies <- coloniesUB(colonies)

# Import raster with distance to colonies
distCol <- stack("data/out/environment/static/distToColony.grd")


#-----------------------------------------------------------------
# Get presence/absence data per colony and link to distance to colony
#-----------------------------------------------------------------

dist_list <- list()

for (i in 1:length(cols)){
  
  # subset data per colony
  icol <- cols[i]
  idata <- dplyr::filter(data, colony == icol)
  
  # select raster of distance to colony
  idist <- distCol[[which(names(distCol)==icol)]]
  names(idist) <- "CDIST"
  
  ## Create an absence map
  rabs <- idist
  rabs[!is.na(rabs)] <- 0
  
  ## Create a presence map with both observed and simulated trips
  rpres <- rasterize(cbind(idata$lon, idata$lat), idist)
  rpres <- rpres/rpres
  
  ## Create presence/absence
  rpa <- sum(rpres, rabs, na.rm=TRUE)
  rpa <- mask(rpa, rabs)
  names(rpa) <- "OCC"
  
  ## Prepare data.frame
  s <- stack(rpa, idist)  # combine
  distdf <- data.frame(na.omit(values(s)))
  
  # appent to list
  dist_list[[i]] <- distdf
}


# combine data from all colonies
dist <- rbindlist(dist_list)


#-----------------------------------------------------------------
# Fit binomial models
#-----------------------------------------------------------------

# if the dataset is too large (e.g. DIO_CHI) generate a random subsample without replacement
if(nrow(dist) > 40000) dist <- sample_n(dist, size = 40000, replace = FALSE)


# Fitted binomial models with a smooth, monotonic decreasing constraint (see Hindell 2020)
# Check: https://github.com/SCAR/RAATD/blob/master/Code/runAvailabilityModels_04.R
# For agazella: https://github.com/SCAR/RAATD/blob/master/Code/runAvailabilityModels_CRAS%26WESE.R
scamMod <- scam(formula = OCC ~ s(CDIST, bs="mpd"),  # Monotone decreasing P-splines
                family = binomial,
                data = dist)

# save
saveRDS(scamMod, paste0(output_data, "/", sp_code, "_accessScam.RDS"))


# Alternative solution is use GLM
# Results are very similar
# glmMod <- glm(OCC ~ CDIST, data = dist, family = binomial)
# saveRDS(glmMod, paste0(output_data, "/", sp_code, "_accessGLM.RDS"))

# plot
#dfpred <- data.frame(CDIST = seq(1, 1694453, 1000))
#dfpred$pre <- predict.scam(scamMod, newdata = dfpred, type="response")
#dfpred$pre <- predict.glm(glmMod, newdata = dfpred, type="response")
#plot(dfpred$CDIST, dfpred$pre)

#-----------------------------------------------------------------
# Predict model
#-----------------------------------------------------------------

## Predict model to each colony
accessCol <- stack()
for (i in 1:nlayers(distCol)){
  
  icol <- subset(distCol, i)
  iname <- names(icol)  # need to replace the name so we keep for later
  names(icol) <- "CDIST"  # replace name
  
  # predict model
  rpred <- predict(icol, scamMod, type="response")
  names(rpred) <- iname
  accessCol <- stack(accessCol, rpred)
}


#-----------------------------------------------------------------
# Combine predictions for all colonies
#-----------------------------------------------------------------

# Combine all colonies (unweighted)
# maximum availability value for a given cell across colonies (Hindell et al 2020)
access <- calc(accessCol, fun=max)

# Weighted average
# Use max population size
selcol <- colonies[colonies$colony %in% names(accessCol),]
w <- selcol$col_pop_max
w <- w/sum(w)  # scale weights
accessW <- weighted.mean(accessCol, w=w)

# Use log-transformation of max population size
wsq <- sqrt(w)
wsq <- wsq/sum(wsq)  # scale weights
accessWsq <- weighted.mean(accessCol, w=wsq)

# Log-transform weighted and normalize from 0 to 1
logtransform <- log(accessW)
accessWlgnorm <- (logtransform - minValue(logtransform)) / (maxValue(logtransform) - minValue(logtransform))

# Combine into a stack and export
accessibility <- stack(access, accessW, accessWsq, accessWlgnorm)
names(accessibility) <- c("unweighted", "weighted", "sqrtWeighted", "normalized")





#-----------------------------------------------------------------
# Plot
#-----------------------------------------------------------------

## Import landmask
data(countriesHigh, package = "rworldxtra", envir = environment())
e <- extent(-8, 17, 33, 46) # raster extent
land <- crop(countriesHigh, e)

# to plot land in stack we need to create a function 
fun <- function() {
  plot(land, col="grey80", border="grey60", add=TRUE)
  points(selcol$lon, selcol$lat, pch=19, col="red", cex=0.5)
  box()
}

# export plot
pngfile <- paste0(output_data, "/", sp_code, "_access.png")
png(pngfile, width=1000, height=600, res=100)
plot(accessibility, zlim=c(0,1), col = viridis(100), addfun = fun)
dev.off()


#-----------------------------------------------------------------
# Export
#-----------------------------------------------------------------

# Export raster
writeRaster(accessibility, paste0(output_data, "/", sp_code, "_access.grd"), bandorder='BIL', overwrite=TRUE)
