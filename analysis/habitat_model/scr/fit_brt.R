#---------------------------------------------------------------------------------------------------
# fit_brt             Fit Species distribution model
#---------------------------------------------------------------------------------------------------





## Load dependencies
library(dplyr)
library(lubridate)
library(rJava) 
library(dismo)
library(data.table)
library(gbm)
library(fmsb)



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

# Transform skewed variables
data$EKE <- log1p(data$EKE)
data$CHL <- log1p(data$CHL)


#-----------------------------------------------------------------
# Boosted Regression Tree
#-----------------------------------------------------------------

mod_code <- "brt"

set.seed(131)

# Fit model
# Uses a default 10-fold cross-validation
mod <- gbm.step(data = data,             # data.frame with data
                gbm.x = vars,          # predictor variables
                gbm.y = "occ",            # response variable
                family = "bernoulli",  # the nature of errror structure
                tree.complexity = 3,   # tree complexity
                learning.rate = 0.01,  # learning rate
                bag.fraction = 0.6)    # bag fraction


# Save model
saveRDS(mod, file = paste0(output_data, "/", sp_code, "_", mod_code, ".rds"))  # save model

# Plot variable contribution using radar plot
var_imp <- summary(mod)$rel.inf
names(var_imp) <- summary(mod)$var
pngfile <- paste0(output_data, "/", sp_code, "_", mod_code, "_var_radar.png")
png(pngfile, width=1000, height=1000, res=150)
radarPlot(var_imp, var_order=vars)
dev.off()

# Plot response curves
pngfile <- paste0(output_data, "/", sp_code, "_", mod_code, "_response.png")
png(pngfile, width=1500, height=1000, res=200)
gbm.plot(mod, n.plots=12)
dev.off()

