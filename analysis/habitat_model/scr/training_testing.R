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
data$date <- ymd(data$date)


#-----------------------------------------------------------------
# Random sample of absences to equal presences. Ratio 1:1
#-----------------------------------------------------------------
# By individual
# Random sample stratified per date
set.seed(134)

# presences
presences <- filter(data, occ==1)
n_occ <- nrow(presences)

# absences
absences <- filter(data, occ==0)
#sel <- sample(1:nrow(absences), n_occ, replace = FALSE, prob = NULL)
#absences <- absences[sel,]
#abs_prop <- nrow(presences)/nrow(absences)
#absences <- stratified(absences, c("id"), abs_prop)


# list of tags
tag_list <- unique(presences$id)

# empty list
abs_list <- list()
cnt <- 1
lower_absences <- 0

# random sample per id and date
for(i in 1:length(tag_list)){
  
  print(paste("Processing tag", i, "from", length(tag_list)))
  
  #get presence/absence for tag i
  iabs <- dplyr::filter(absences, id == tag_list[i])
  ipres <- dplyr::filter(presences, id == tag_list[i])
  
  # get list of dates
  idates <- sort(unique(ipres$date))
  
  # extract absences per date
  for(j in 1:length(idates)){
    
    # get number of presences on date j
    jpres <- dplyr::filter(ipres, date == idates[j])
    n <- nrow(jpres)
    
    # get absences
    jabs <- dplyr::filter(iabs, date == idates[j])
    
    # if lower number of absences than presences, sample from +- 1 date
    if(nrow(jabs) < nrow(jpres)){
      jabs <- dplyr::filter(iabs, date >= idates[j]-3, date <= idates[j]+3)
      lower_absences <- lower_absences + 1
    }
    
    # random sample of absences on date j
    rsel <- sample(1:nrow(jabs), n, replace = FALSE, prob = NULL)
    rabs <- jabs[rsel,]
    
    # append
    abs_list[[cnt]] <- rabs
    cnt <- cnt + 1
  }
}

# combine presence-absences
data <- bind_rows(presences, absences)

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

# resampled data
outfile <- paste0(outdir, "/", sp_code, "_data.csv")
write.csv(data, outfile, row.names = FALSE)
