#---------------------------------------------------------------------------------------------------
# training_testing.R         Split data into training and testing
#---------------------------------------------------------------------------------------------------



#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste0(output_data, "/tracking/", sp_code, "/PresAbs/")
outdir <- paste0(output_data, "/habitat-model/", sp_code)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


#-----------------------------------------------------------------
# Import observations
#-----------------------------------------------------------------
obs_file <- paste0(indir, sp_code, "_observations.csv")
data <- read.csv(obs_file)


#-----------------------------------------------------------------
# Split data
#-----------------------------------------------------------------
# Random subsample of data to reduce amount of data
set.seed(134)
data$id <- 1:nrow(data)  # add id to not overlap
train <- stratified(data, c("occ", "date"), train_prop)
test <- data %>% dplyr::filter(!id %in% train$id)

#-----------------------------------------------------------------
# Export data
#-----------------------------------------------------------------

# training
outfile <- paste0(outdir, "/", sp_code, "_train.csv")
write.csv(train, outfile, row.names = FALSE)

# testing
outfile <- paste0(outdir, "/", sp_code, "_test.csv")
write.csv(test, outfile, row.names = FALSE)
