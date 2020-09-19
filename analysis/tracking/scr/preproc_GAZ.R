#-----------------------------------------------------------------------------------------
# 01_preproc_GAZ.R        Pre-process loggerhead tracking data
#-----------------------------------------------------------------------------------------
# This script pre-processes tracking data. The main goal is to standardize among multiple
# formats (tag manufacturers, custom pre-processing from different labs) and generate a
# common and standardized format to then follow a common workflow
#
# About Anctartic fur seal data:
# Loggerhead input data has been prepared by Lluis Cardona (UB)
#
# All tags have been previously standardized into a common format
#
# All location data correspond to Argos


#---------------------------------------------------------------
# Prepare cluster
#---------------------------------------------------------------
cl <- makeCluster(cores)
registerDoParallel(cl)


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

# input file with batch data
file <- "data/raw/tracking/TelemetriaPPT.xlsx"

# output directory
output_data <- paste0("data/out/tracking/", sp_code, "/L0_locations")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Process metadata
#---------------------------------------------------------------

# import data
df <- read_agazella(file)  # takes time

# summarize data per id
db <- summarizeId(df)



#---------------------------------------------------------------
# 3. Process batch data
#---------------------------------------------------------------

# process each tag 
foreach(i=db$id, .packages=c("dplyr", "ggplot2", "stringr")) %dopar% {

  # subset data and standardize
  id <- i
  dataL0 <- dplyr::filter(df, id == i)
  
  # define "trips"
  dataL0$trip <- dataL0$id
  #dataL0$trip <- segmentOnPort(dataL0$habitat)  # generate segments including periods on land
  #dataL0 <- filter(dataL0, habitat == 2)  # remove periods on land
  #dataL0$trip <- segmentOnPort(dataL0$trip)  # regenerate trip id
  #dataL0$trip <- paste(i, str_pad(dataL0$trip, 3, pad = "0"), sep="_") 
  dataL0 <- dplyr::select(dataL0, id, trip, everything())
  
  # store into individual folder at output path
  out_file <- paste0(output_data, "/", id, "_L0_locations.csv")
  write.csv(dataL0, out_file, row.names = FALSE)
  
  # plot track
  p <- map_argos(dataL0)
  out_file <- paste0(output_data, "/", id, "_L0_locations.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")
}

stopCluster(cl)  # Stop cluster


#---------------------------------------------------------------
# 5. Summarize processed data
#---------------------------------------------------------------

# import all location files
loc_files <- list.files(output_data, full.names = TRUE, pattern = "L0_locations.csv")
df <- readTrack(loc_files)

# summarize data per animal id
idstats <- summarizeId(df)

# export table
out_file <- paste0(output_data, "/", sp_code, "_summary_id.csv")
write.csv(idstats, out_file, row.names = FALSE)


print("Pre-processing ready")





