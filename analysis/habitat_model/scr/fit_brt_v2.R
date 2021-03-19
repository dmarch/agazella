#---------------------------------------------------------------------------------------------------
# fit_brt             Fit Species distribution model
#---------------------------------------------------------------------------------------------------
# create a grid of hyper-parameters (learning rate, depth, min obs in nodes, etc...)
# and build a full model on each combination of parameters in parallel. 

source("scr/utilsGbm.R")
source("scr/utilsGeneral.R")


# to make optimization faster: need to reduce amount of data. options here:
# - define training (70%) and testing (30%). how to partition?
# - select one location per day to reduce spatio-temporal autocorrelation
# - reduce number of pseudo-absences: Simulate 20 tracks (Hazen et al. 2018, Reisinger et al. 2018)
# - optimize using a random subsample of the data (http://www.int-res.com/articles/meps_oa/m608p263.pdf)
# - use faster lr combinations (0.05, 0.01, 0.005)
# and in case of ties, prioritized
# models with larger learning rates, smaller tree complexities and fewer trees to reduce overfitting (Elith et al. 2008)
# ensure that at least 1,000 trees were included in the final model configuration (Elith et al., 2008).                                                                                                     

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste0(output_data, "/tracking/", sp_code, "/PresAbs/")
outdir <- paste(output_data, "habitat-model", sp_code, mod_code, sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


#-----------------------------------------------------------------
# Prepare data
#-----------------------------------------------------------------

# Import observations
obs_file <- paste0(indir, sp_code, "_observations.csv")
#obs_file <- "data/GAZ_observations.csv"
data <- read.csv(obs_file)

# Transform skewed variables
data$EKE <- log1p(data$EKE)
data$CHL <- log1p(data$CHL)

# Generate Random Number
# We also included a random number between 1 and 100 to serve as an indicator for variables that
# have influence greater or less than random (Scales et al., 2017; Soykan, Eguchi, Kohin, & Dewar, 2014);
# only variables with influence greater than the random number were included in the final models.
data$RN <- sample.int(100, size=nrow(data), replace=T, prob=NULL)
vars <- c(vars, "RN")

# Training and testing
# we cross‐validated the models by running a training model by randomly choosing 75% of the entire dataset,
# then comparing model predictions against the remaining 25% of the data, while maintaining the same ratio of presences
# to pseudo‐absences. (Maxwell 2019)

# Random subsample of data to reduce amount of data
set.seed(134)
sdata <- stratified(data, c("occ", "id", "day"), 0.25)


#-----------------------------------------------------------------
# Boosted Regression Tree
#-----------------------------------------------------------------

mod_code <- "brt"

set.seed(131)
ini.nt = 1000
max.nt = 20000
step.nt = 1000

comb <- expand.grid(lr=c(0.005, 0.01, 0.05), tc=c(1,3,5), bf=c(0.5,0.6,0.7)) #combination
tree.list <- seq(ini.nt,max.nt,by=step.nt) #list of trees for evaluation
#cv.deviance <- matrix(data=NA,nrow=length(tree.list),ncol=nrow(comb))#rep(0,100) #matrix of ntrees.steps vs combinations

#pred.deviance <- matrix(data=NA,nrow=length(tree.list),ncol=nrow(comb))#rep(0,100) #matrix of ntrees.steps vs combinations


## Prepare clusters
cores <- 10  # detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)



all_list <- foreach(i=1:nrow(comb), .packages=c("dismo", "gbm", "dplyr")) %dopar% {
  #print(paste("Combination",i,"of",nrow(comb),sep=" "))
  
  # Fit model
  # Uses a default 10-fold cross-validation
  # faster learning rate means larger values
  mod <- gbm.step(data = sdata,             # data.frame with data
                  gbm.x = vars,          # predictor variables
                  gbm.y = "occ",            # response variable
                  family = "bernoulli",  # the nature of errror structure
                  tree.complexity = comb$tc[i],   # tree complexity
                  learning.rate = comb$lr[i],  # learning rate
                  bag.fraction = comb$bf[i],    # bag fraction
                  n.trees = ini.nt, step.size = step.nt, max.trees = max.nt)    

  
  # Keep CV parameters
  mod_out <- data.frame(
    tree.complexity = mod$interaction.depth,
    learning.rate = mod$shrinkage,
    bag.fraction = mod$bag.fraction,
    n.trees = mod$n.trees,
    AUC = mod$self.statistics$discrimination,
    cv.AUC = mod$cv.statistics$discrimination.mean,
    deviance = mod$self.statistics$mean.resid,
    cv.deviance = mod$cv.statistics$deviance.mean
  ) 

  # keep deviance values for all trees
  cv_deviance <- mod$cv.values
  cv_deviance <- c(cv_deviance, rep(NA, length(tree.list) - length(cv_deviance)))  #fill with NA
  
  list(mod_out = mod_out, cv_deviance = cv_deviance)
}


## combine outputs
mod_out <- rbindlist(foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$mod_out)
cv_deviance <- bind_cols(foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$cv_deviance)
names(cv_deviance) <- paste0("comb", 1:nrow(comb))
cv_deviance$ntrees <- tree.list

## stop clusters
stopCluster(cl)

## export outputs
outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_optim_params.csv")
write.csv(mod_out, outfile, row.names = FALSE)

outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_cv_deviance.csv")
write.csv(cv_deviance, outfile, row.names = FALSE)





# Confidence intervals were calculated across 10 boosted regression tree
# model fits to account for model stochasticity (Hazen et al. 2018)


# For each habitat selection model (i.e., each life-history stage of each species), we fitted the model 50 times.
# For each of the 50 iterations, we used the parameter values chosen for the final model, but we sampled
# half the data (with replacement) to fit the model. (Hindell et al. 2020)

