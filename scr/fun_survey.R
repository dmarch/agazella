#------------------------------------------------------------------------------------
# fun_survey.R    Suite of function for processing survey data
#------------------------------------------------------------------------------------
# This script contains the following custom functions:
#
# spt_overlap     Spatio-temporal overlap of grid cells


#------------------------------------------------------------------------------------
# spt_overlap     Spatio-temporal overlap of grid cells
#------------------------------------------------------------------------------------
spt_overlap <- function(abs_cell, abs_date, pres_df, temporal_thrs, grid){
  # Given a cell number and date for an absence, this function checks the overlap with presences.
  # For the temporal criteria, it first filter all presence within the temporal threshold (in days).
  # Then, for the filtered cells, the function checks if they are adhjacent to the target cell.
  #
  # absence and presence cell number have to be originated from the same raster (grid)
  
  library(dplyr)
  library(raster)
  
  # check if there are presence for a given date, considering temporal window
  ipres <- dplyr::filter(pres_df, date >= abs_date - temporal_thrs, date <= abs_date + temporal_thrs)
  if(nrow(ipres)==0) keep <- TRUE
  
  # check if there are adjacent cells
  if(nrow(ipres)>0){
    adj <- adjacent(grid, cells = as.numeric(abs_cell), directions = 8, pairs = FALSE,
                    include = TRUE, target = ipres$cell)
    
    if(length(adj)==0) keep <- TRUE
    if(length(adj)>0) keep <- FALSE
  }
  
  return(keep)
}
#------------------------------------------------------------------------------------
