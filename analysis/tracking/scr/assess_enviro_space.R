#----------------------------------------------------------------------------------------------
# assess_enviro_space.R        Assess environmental space of simulations
#----------------------------------------------------------------------------------------------
# In order to estimate the optimal number of simulations for the habitat model, we follow
# the same approach from Queiroz et al. 2016 and Hindell et al. 2020
# To determine the optimal number of simulated pseudo-absences we calculated,
# for each environmental variable, the mean and standard deviation of increasing numbers of
# simulated tracks.


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
# Output folder is common for all tracking species

input_data <- paste0("data/out/tracking/", sp_code, "/L3_simulations")
output_data <- paste0("results/environmental_space")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Import data
#---------------------------------------------------------------

# List files from input data
loc_files <- list.files(input_data, full.names = TRUE, pattern="L3_locations.csv")
sim <- readTrack(loc_files)

# Get number of simulations
nsim <- unique(sim$nsim)

# Environmental varriables
vars <- all_vars


#---------------------------------------------------------------
# 3. Calculate accumulated metrics
#---------------------------------------------------------------

# Transform some variables
sim$BAT <- abs(sim$BAT)
sim$EKE <- log1p(sim$EKE)
sim$CHL <- log1p(sim$CHL)

# Prepare loop
data_list <- list()  # Create empty list
cnt <- 0  # set counter to 0

# Loop for each simulation and variable
for(i in nsim){
  for(j in 1:length(vars)){
    
    # subset data of all tags for simulation (i) and variable (j)
    # subset accumulates with the number of simulations
    subsim <- sim %>%
      filter(nsim <= i) %>%
      dplyr::select(vars[j]) %>%
      data.frame()  # NEW ADDED
    
    # calculate mean and sd
    avg <- mean(subsim[,1], na.rm=TRUE)
    sd <- sd(subsim[,1], na.rm=TRUE)
    var <- var(subsim[,1], na.rm=TRUE)
    
    # keep result as data.frame into a list
    df <- data.frame(nsim = i, var = vars[j], mean = avg, sd = sd, var= var)
    cnt <- cnt+1
    data_list[[cnt]] <- df
  }
}

## Combine data into a single data.frame
dat <- rbindlist(data_list)



# Define dimentions based on number of variables
ncol <- 3
nrow <- ceiling(length(vars)/ncol)


# define output file plot
pngfile <- paste0(output_data,  "/", sp_code, "_environmental_space.pdf")
#png(file = pngfile, width=2000, height=2200, res=200)
pdf(file = pngfile, width=ncol*5, height=nrow*2.5)

# create layout
mat_vals <- c(1:(ncol*nrow))
layout.matrix <- matrix(mat_vals, nrow = nrow, ncol = ncol, byrow=TRUE)
layout(mat = layout.matrix,
       heights = rep(1, nrow), # Heights of the rows
       widths = rep(2, ncol)) # Widths of the two columns

# plot variables
for (i in 1:length(vars)){
  
  # subset data
  ivar = vars[i]
  sdat <- filter(dat, var == ivar)
  
  # plot
  # In ggplot2 seems not feasible to adjust secondary y axis lims
  two.y(x=sdat$nsim,
        y1=sdat$mean,
        y2=sdat$sd,
        var.name = ivar)
}

# close plot device
dev.off()



print("Assessment of evironmental space ready")  
