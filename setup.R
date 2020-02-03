#--------------------------------------------------------------------------------
# setup.R         Setup project
#--------------------------------------------------------------------------------



# 1. Install required packages
# Set required packages and use function to check if installed and load them
required_packages <- c("dplyr", "openxlsx", "move", "moveVis", "SDLfilter", "data.table", "stringr", "lubridate")
install_and_load <- function(Required_Packages){
  # source: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];
  
  if(length(Remaining_Packages)) 
  {
    install.packages(Remaining_Packages);
  }
  for(package_name in Required_Packages)
  {
    library(package_name,character.only=TRUE,quietly=TRUE);
  }
}
install_and_load(required_packages)

# 2. Create data paths
# The project involves creating three main folders into the working directory to manage the data:
ifelse(!dir.exists("data"), dir.create("data"), "Folder 'data' exists already")  # contains raw data
ifelse(!dir.exists("temp"), dir.create("temp"), "Folder 'temp' exists already")  # contains temporary files (required for further steps)
ifelse(!dir.exists("out"), dir.create("out"), "Folder 'out' exists already")  # contains output files
ifelse(!dir.exists("out/proc"), dir.create("out/proc"), "Folder 'proc' exists already")  # contains output files


# 3. Store raw data into /data folder

# Trackin data
# data/TelemetriaPPT.xlsx