

# 1. Combine all observed tracks. We avoid placing an absence for individual (A) in an cell (i) and day (j),
# where there is the presence of indivdiaul (B).
# 2. Combine all pseudo-absences.
# 3. Get cell (i) and day (j) for observed tracks. Removed duplicates: considers temporal and spatial autocorrelation of tracking data.
# Note individual variability is not considered in this study.
# 4. Remove pseudo-absences that overlap in time and space with observations.









#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

ssm_data <- paste0(output_data, "/tracking/", sp_code, "/L2_locations")
sim_data <- paste0(output_data, "/tracking/", sp_code, "/L2_simulations")
outdir <- paste0(output_data, "/tracking/", sp_code, "/PresAbs")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



#---------------------------------------------------------------
# 2. Import presence and absences
#---------------------------------------------------------------


# Presence data (observed state-space models)
loc_files <- list.files(ssm_data, full.names = TRUE, pattern="L2_locations.csv")
pres <- readTrack(loc_files)
pres$date <- as.Date(pres$date)  # transform date-time into date format

# Absence data (simulations)
loc_files <- list.files(sim_data, full.names = TRUE, pattern="L2_locations.csv")
abs <- readTrack(loc_files)

# Filter data by number of simulations
abs <- filter(abs, nsim <= sim_n)

# transform date-time into date format
abs$date <- as.Date(abs$date)



#---------------------------------------------------------------
# 2. Generate oceanmask
#---------------------------------------------------------------
# Create a ocean mask to grid all observations
# It is based on the following parameters:
# res: resolution
# ext: extent estimated from the data

# Import oceanmask
#oceanmask <- raster( paste0(output_data, "/stack_daily/2019/02/20190220_enviro.grd"))
#grid <- oceanmask+0  # this makes the raster to be in memory and make simulations faster

xmin <- floor(min(min(pres$lon), min(abs$lon)))
xmax <- ceiling(max(max(pres$lon), max(abs$lon)))
ymin <- floor(min(min(pres$lat), min(abs$lat)))
ymax <- ceiling(max(max(pres$lat), max(abs$lat)))

ext <- extent(xmin, xmax, ymin, ymax)
grid <- raster(ext, res = res, crs = crs("+proj=longlat +datum=WGS84"))


#---------------------------------------------------------------
# 3. Generate presences
#---------------------------------------------------------------

# Extract cell ID from raster for each observation
pres$cell <- cellFromXY(grid, cbind(pres$lon, pres$lat))

# Transform observation to presence by cell and date
cpres <- pres %>%
  dplyr::group_by(cell, date) %>%
  dplyr::summarize(occ = 1,
            n = n())

# Get raster coordinates
xy <- xyFromCell(grid, cpres$cell)
cpres$lon <- xy[,"x"]
cpres$lat <- xy[,"y"]


#---------------------------------------------------------------
# 4. Generate absences
#---------------------------------------------------------------

# Extract cell ID from raster for each observation
abs$cell <- cellFromXY(grid, cbind(abs$lon, abs$lat))

# Transform observation to absence by cell and date
cabs <- abs %>%
  dplyr::group_by(cell, date) %>%
  dplyr::summarize(occ = 0,
            n = n())

# Get raster coordinates
xy <- xyFromCell(grid, cabs$cell)
cabs$lon <- xy[,"x"]
cabs$lat <- xy[,"y"]




#--------------------------------------------------------------------------
# FILTER OUT ABSENCES BY SPATIAL AND TEMPORAL CRITERIA
# We overlap absence with the presence bins. If there was a presence, we remove such absence
# separate non-overlappingabsence and presence locations
#--------------------------------------------------------------------------

# Register number of cores to use in parallel
cl <- parallel::makeCluster(cores) # 10 cores work at around 63% CPU (no major problem with RAM)
registerDoParallel(cl)

# for each absence, check if there is a presence in adjacent cells within the temporal period defined
# if there is a match, remove absence. if not, keep it.
# Note: This part of the code is computer intensive and time consuming. Using parallel works fine.
keep <- foreach(i=1:nrow(cabs), .packages=c("dplyr", "raster")) %dopar% {
  spt_overlap(abs_cell = cabs$cell[i], abs_date = cabs$date[i],
              pres_df = cpres, temporal_thrs, grid)
}

# Stop cluster
parallel::stopCluster(cl)

# filter out absences that match presences
cabs$keep <- unlist(keep)
cabs <- filter(cabs, keep == TRUE) %>%
  dplyr::select(-keep)


#--------------------------------------------------------------------------
# EXPORT DATA
#--------------------------------------------------------------------------

# combine presence and absence into a single data.frame and save
comb <- rbind(cpres, cabs)
comb$sp_code <- sp_code
comb <- dplyr::select(comb, sp_code, cell, lon, lat, date, occ)

# export to species folder
outfile <- paste0(outdir, "/", sp_code, "_L2_PresAbs.csv")
write.csv(comb, outfile, row.names = FALSE)
