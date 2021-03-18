#---------------------------------------------------------------------------------------------------
# fit_brt             Fit Species distribution model
#---------------------------------------------------------------------------------------------------
# create a grid of hyper-parameters (learning rate, depth, min obs in nodes, etc...)
# and build a full model on each combination of parameters in parallel. 

source("scr/utilsGbm.R")
source("scr/utilsGeneral.R")


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
data <- read.csv(obs_file)

# Transform skewed variables
data$EKE <- log1p(data$EKE)
data$CHL <- log1p(data$CHL)


#-----------------------------------------------------------------
# Boosted Regression Tree
#-----------------------------------------------------------------

mod_code <- "brt"

set.seed(131)

# subset data
sdata <- data[seq(1, nrow(data), 50),]


ini.nt = 1000
max.nt = 10000
step.nt = 1000

comb <- expand.grid(lr=c(0.005, 0.01, 0.5), tc=c(1,3,5), bf=c(0.5,0.6,0.7)) #combination
tree.list <- seq(ini.nt,max.nt,by=step.nt) #list of trees for evaluation
#cv.deviance <- matrix(data=NA,nrow=length(tree.list),ncol=nrow(comb))#rep(0,100) #matrix of ntrees.steps vs combinations

#pred.deviance <- matrix(data=NA,nrow=length(tree.list),ncol=nrow(comb))#rep(0,100) #matrix of ntrees.steps vs combinations


## Prepare clusters
cores <- 2  # detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

#for (i in 1:nrow(comb)){

all_list <- foreach(i=1:3, .packages=c("dismo", "gbm", "dplyr")) %dopar% {
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
  if(is.null(cv_deviance)) cv_deviance <- rep(NA, (max.nt-ini.nt)/step.nt)
  
  list(mod_out = mod_out, cv_deviance = cv_deviance)
}


## combine outputs
mod_out <- rbindlist(foreach(i=1:3) %dopar% all_list[[i]]$mod_out)
cv_deviance <- rbindlist(foreach(i=1:3) %dopar% all_list[[i]]$cv_deviance, fill=TRUE)

## stop clusters
stopCluster(cl)

## export outputs

