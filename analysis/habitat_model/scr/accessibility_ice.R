#---------------------------------------------------------------------
# accessibility
#---------------------------------------------------------------------


source("setup.R")
source("scr/fun_track_reading.R")  # read multiple tracking data formats
library(scam)
library(pals)


sp_code <- "GAZ"  # species code

# Define number of bootstrap models
n.boot <- 50  # number of model fits

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

# input data paths
indir <- paste0(output_data, "/habitat-model-v2/", sp_code, "/")
stack_repo <- paste0(output_data, "/stack_daily")

# output data paths
outdir <- paste(output_data, "habitat-model-v2", sp_code, "access_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

#-----------------------------------------------------------------
# Prepare data
#-----------------------------------------------------------------

# import observaions
obs_file <- paste0(indir,"/", sp_code, "_data.csv")
data <- read.csv(obs_file)
data$date <- ymd(data$date)

# Create dates
dates <- seq.Date(min(data$date), max(data$date), by="day")  # define sequence



#-----------------------------------------------------------------
# Get presence/absence data and link to distance to ice edge
#-----------------------------------------------------------------

dist_list <- list()

for(i in 1:length(dates)){
  
  print(paste("Processing date", i, "from", length(dates)))
  
  # Get time information
  date <- dates[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  
  # select tracking data
  idata <- dplyr::filter(data, date == dates[i])
  
  # Locate file
  pat <- paste0(format(date, "%Y%m%d"), "_enviro.grd")
  grdfile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
  
  # Import environmental stack
  e <- raster::stack(grdfile)
  idist <- e$EDGE
  idist[idist<0] <- NA
  
  
  ## Create an absence map
  rabs <- idist
  rabs[!is.na(rabs)] <- 0

  ## Create a presence map with both observed and simulated trips
  rpres <- rasterize(cbind(idata$lon, idata$lat), idist)
  rpres <- rpres/rpres
  
  ## Create presence/absence
  rpa <- sum(rpres, rabs, na.rm=TRUE)
  rpa <- raster::mask(rpa, rabs)
  names(rpa) <- "OCC"
  
  ## ncell raster
  rcell <- e$D2COL
  rcell[] <- 1:ncell(rcell)
  names(rcell) <- "CELLID"
  
  ## Prepare data.frame
  s <- stack(rpa, idist, e$D2COL/1000, rcell)  # combine
  distdf <- data.frame(na.omit(values(s)))
  distdf$date <- date

  # appent to list
  dist_list[[i]] <- distdf
}


# combine data
dist <- rbindlist(dist_list)
dist$month <- month(dist$date)

# select presences
dist_pres <- filter(dist, OCC == 1)
cell_with_pres <- dist_pres %>%
  distinct(CELLID)

#select absences

# remove absence cells with at any observed or simulate location
# also, remove duplicated absences within same month
dist_abs <- dist %>%
  dplyr::filter(OCC == 0, !CELLID %in% cell_with_pres$CELLID) %>%
  distinct(CELLID, month, .keep_all = TRUE)

# subsample absences
dist_abs <- sample_n(dist_abs, size = 40000-nrow(dist_pres), replace = FALSE)
dist_abs <- sample_n(dist_abs, size = nrow(dist_pres), replace = FALSE)

# combine
dist <- bind_rows(dist_pres, dist_abs)

# export dataset
outfile <- paste(outdir, "data.csv", sep="/")
write.csv(dist, outfile, row.names = FALSE)

#-----------------------------------------------------------------
# Fit binomial models (bootstrap)
#-----------------------------------------------------------------

# read data
outfile <- paste(outdir, "data.csv", sep="/")
dist <- read.csv(outfile)


## Prepare clusters
cores <- 50
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i=1:n.boot, .packages=c("splitstackshape", "scam")) %dopar% {
  
  # sampled half the data (with replacement) to fit the model (Hindell et al. 2020)
  idist <- stratified(dist, c("OCC", "month"), 0.5, replace = TRUE)
  
  # Fitted binomial models with a smooth, monotonic decreasing constraint (see Hindell 2020)
  # Check: https://github.com/SCAR/RAATD/blob/master/Code/runAvailabilityModels_04.R
  # For agazella: https://github.com/SCAR/RAATD/blob/master/Code/runAvailabilityModels_CRAS%26WESE.R
  scamMod <- scam(formula = OCC ~ s(EDGE, bs="mpd"),  # Monotone decreasing P-splines
                  family = binomial,
                  data = dist)
  
  # store model
  outfile <- paste0(outdir, "/", str_pad(i, 2, pad = "0"), "_", sp_code, "access_boot.rds")
  saveRDS(scamMod, file = outfile)  # save model
}

## stop clusters
stopCluster(cl)

