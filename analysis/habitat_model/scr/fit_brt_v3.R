#---------------------------------------------------------------------------------------------------
# fit_brt             Fit Species distribution model
#---------------------------------------------------------------------------------------------------
# create a grid of hyper-parameters (learning rate, depth, min obs in nodes, etc...)
# and build a full model on each combination of parameters in parallel. 

# to make optimization faster: need to reduce amount of data. options here:
# - select one location per day to reduce spatio-temporal autocorrelation
# - reduce number of pseudo-absences: Simulate 20 tracks (Hazen et al. 2018, Reisinger et al. 2018)
# - optimize using a random subsample of the data (http://www.int-res.com/articles/meps_oa/m608p263.pdf)
# - use faster lr combinations (0.05, 0.01, 0.005)
# and in case of ties, prioritized
# models with larger learning rates, smaller tree complexities and fewer trees to reduce overfitting (Elith et al. 2008)
# ensure that at least 1,000 trees were included in the final model configuration (Elith et al., 2008).                                                                                                     


mod_code <- "brt"

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste0(output_data, "/habitat-model/", sp_code, "/")
outdir <- paste(output_data, "habitat-model", sp_code, mod_code, sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


#-----------------------------------------------------------------
# Prepare data
#-----------------------------------------------------------------
# We also included a random number between 1 and 100 to serve as an indicator for variables that
# have influence greater or less than random (Scales et al., 2017; Soykan, Eguchi, Kohin, & Dewar, 2014);
# only variables with influence greater than the random number were included in the final models.

## Data
obs_file <- paste0(indir,"/", sp_code, "_data.csv")
train <- read.csv(obs_file)

# Transform skewed variables
train$EKE <- log1p(train$EKE)
train$CHL <- log1p(train$CHL)

# Transform ice edge distance (change negative values to zero)
train$EDGE[train$EDGE < 0] <- 0

# Generate Random Number
train$RN <- sample.int(100, size=nrow(train), replace=T, prob=NULL)


# ## Training
# 
# # Import observations
# train_file <- paste0(indir, sp_code, "_train.csv")
# train <- read.csv(train_file)
# 
# # Transform skewed variables
# train$EKE <- log1p(train$EKE)
# train$CHL <- log1p(train$CHL)
# 
# # Generate Random Number
# train$RN <- sample.int(100, size=nrow(train), replace=T, prob=NULL)
# 
# 
# ## Testing (same steps)
# 
# # Import observations
# test_file <- paste0(indir, sp_code, "_test.csv")
# test <- read.csv(test_file)
# 
# # Transform skewed variables
# test$EKE <- log1p(test$EKE)
# test$CHL <- log1p(test$CHL)


#-----------------------------------------------------------------
# Boosted Regression Tree - Optimization of hyper-parameters
#-----------------------------------------------------------------
# To improve:
#For each combination: keep list with variable importance, gbm.simplify is very low, so we can use the criteria of being above the RN

# Add the RN to the list of variables
vars <- c(vars, "RN")

# Define number of trees
ini.nt = 50
max.nt = 20000
step.nt = 50
tree.list <- seq(ini.nt,max.nt,by=step.nt) #list of trees for evaluation

# Define combination of hyper-parameters
comb <- expand.grid(lr=c(0.001, 0.005, 0.01, 0.05), tc=c(1,3,5), bf=c(0.5, 0.6, 0.7)) #combination


## Prepare clusters
cores <- nrow(comb)  # detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

set.seed(131)
all_list <- foreach(i=1:nrow(comb), .packages=c("dismo", "gbm", "dplyr")) %dopar% {

  # Fit model
  # Uses a default 10-fold cross-validation
  # faster learning rate means larger values
  mod <- tryCatch(
                  gbm.step(data = train,             # data.frame with data
                  gbm.x = vars,          # predictor variables
                  gbm.y = "occ",            # response variable
                  family = "bernoulli",  # the nature of errror structure
                  tree.complexity = comb$tc[i],   # tree complexity
                  learning.rate = comb$lr[i],  # learning rate
                  bag.fraction = comb$bf[i],    # bag fraction
                  n.trees = ini.nt, step.size = step.nt, max.trees = max.nt)   
                  , error = function(e) return(NA))
  
  
  if(!is.na(mod)) {
    # Keep CV parameters
    mod_out <- data.frame(
      tree.complexity = mod$interaction.depth,
      learning.rate = mod$shrinkage,
      bag.fraction = mod$bag.fraction,
      n.trees = mod$n.trees,
      AUC = mod$self.statistics$discrimination,
      cv.AUC = mod$cv.statistics$discrimination.mean,
      deviance = mod$self.statistics$mean.resid,
      cv.deviance = mod$cv.statistics$deviance.mean,
      PER = (1-mod$self.statistics$mean.resid/mod$self.statistics$mean.null)*100,
      cv.PER = (1-mod$cv.statistics$deviance.mean/mod$self.statistics$mean.null)*100
    ) 
    
    # keep deviance values for all trees
    cv_deviance <- mod$cv.values
    cv_deviance <- c(cv_deviance, rep(NA, length(tree.list) - length(cv_deviance)))  #fill with NA
    
    # selected variables
    pred_order <- summary(mod)$var
    rn_position <- which(pred_order == "RN")
    pred_list <- as.character(pred_order[1:(rn_position-1)])
    
    
    list(mod_out = mod_out, cv_deviance = cv_deviance, pred_list = pred_list)
  }

}


## combine model outputs
mod_list <- foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$mod_out
mod_list[!lengths(mod_list)] <-  list(data.frame(tree.complexity = NA))
mod_out <- rbindlist(mod_list, fill = TRUE)
mod_out <- bind_cols(comb,
               dplyr::select(mod_out, -c(tree.complexity, learning.rate, bag.fraction))) %>%
            dplyr::mutate(id = 1:n())


## combine deviance outputs
deviance_list <- list()
for(i in 1:nrow(mod_out)){
  # extract deviance data
  dev <- all_list[[i]]$cv_deviance
  
  # check that there is no null data
  if(is.null(dev)) dev <- rep(NA,length(tree.list))
  
  # make data.frame with number of trees
  df <- data.frame(id = mod_out$id[i], lr = mod_out$lr[i], tc = mod_out$tc[i], bf = mod_out$bf[i], ntrees = tree.list, cv_deviance = dev)
  deviance_list[[i]] <- df
}
cv_deviance <- rbindlist(deviance_list)


## get selected variables
predict_list <- foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$pred_list

## stop clusters
stopCluster(cl)


## plot profiles
p <- ggplot(data = cv_deviance) +
  geom_line(data = dplyr::rename(cv_deviance, comb = id), aes(x = ntrees, y = cv_deviance, group = comb), color = "grey80") +
  geom_line(aes(x = ntrees, y = cv_deviance, group = id), color = "firebrick3") +
  facet_wrap(id ~.,) +
  theme_article()
outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_optim_params.png")
ggsave(outfile, p, width=25, height=14, units="cm", dpi=300)

## export outputs
outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_optim_params.csv")
write.csv(mod_out, outfile, row.names = FALSE)

outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_cv_deviance.csv")
write.csv(cv_deviance, outfile, row.names = FALSE)

outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_predlist.rds")
saveRDS(predict_list, outfile)

#-----------------------------------------------------------------
# Boosted Regression Tree - Fit full model
#-----------------------------------------------------------------

mod_out <- read.csv(paste0(outdir, "/", sp_code, "_", mod_code, "_optim_params.csv"))
predict_list <- readRDS(paste0(outdir, "/", sp_code, "_", mod_code, "_predlist.rds"))

select_model_id <- 36

tc <- mod_out$tc[select_model_id]
lr <- mod_out$lr[select_model_id]
bf <- mod_out$bf[select_model_id]
ntrees <- mod_out$n.trees[select_model_id]
pred_list <- vars[vars %in% predict_list[[select_model_id]]]

# remove variables not selected
# fir BRT with selected parameters
mod_full <- dismo::gbm.fixed(data = train,             # data.frame with data
                gbm.x = pred_list,          # predictor variables
                gbm.y = "occ",            # response variable
                family = "bernoulli",  # the nature of errror structure
                tree.complexity = tc,   # tree complexity
                learning.rate = lr,  # learning rate
                bag.fraction = bf,    # bag fraction
                n.trees = ntrees) 

# Save model
saveRDS(mod_full, file = paste0(outdir, "/", sp_code, "_", mod_code, ".rds"))  # save model
mod_full <- readRDS(paste0(outdir, "/", sp_code, "_", mod_code, ".rds"))

# Plot variable contribution using radar plot
var_imp <- summary(mod_full)$rel.inf
names(var_imp) <- summary(mod_full)$var
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_var_radar.png")
png(pngfile, width=1000, height=1000, res=150)
radarPlot(var_imp, var_order=pred_list)
dev.off()

# Plot variable contribution using bar plot
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_var_influence.png")
png(pngfile, width=1000, height=1000, res=150)
ggBRT::ggInfluence(mod_full, show.signif = F, col.bar = "skyblue3")
dev.off()

# Plot response curves
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_response.png")
png(pngfile, width=1500, height=1500, res=200)
#dismo::gbm.plot(mod_full, smooth=TRUE, n.plots=6, rug=FALSE, common.scale=TRUE, plot.layout=c(2, 3))
names(mod_full$gbm.call)[1] <- "dataframe"
ggBRT::ggPD(mod_full, n.plots =6, smooth = F, rug = F, ncol=2, col.line = "skyblue3")
dev.off()


#-----------------------------------------------------------------
# Boosted Regression Tree - Interactions
#-----------------------------------------------------------------
# seems it is not working with fixed objects, try to gbm.step

# there is a bug to get interaction for gbm.fixed, unlike gbm.stem
# changing the name of one variable fixes the problem
names(mod_full$gbm.call)[1] <- "dataframe"

find.int <- dismo::gbm.interactions(mod_full)
find.int$interactions
find.int$rank.list

dismo::gbm.perspec(mod_full, 11, 9)
gbm.perspec(mod_full, 12, 9)
gbm.perspec(mod_full, 12, 10)
gbm.perspec(mod_full, 13, 3)

#-----------------------------------------------------------------
# BRT - Predict on testing dataset
#-----------------------------------------------------------------

# # predict
# preds <- predict.gbm(mod_full, test, n.trees=mod_full$gbm.call$best.trees, type="response")
# 
# # calculate deviance
# calc.deviance(obs=test$occ, pred=preds, calc.mean=TRUE)
# 
# d <- cbind(test$occ, preds)
# pres <- d[d[,1]==1, 2]
# abs <- d[d[,1]==0, 2]
# e <- evaluate(p=pres, a=abs)
# 
# plot(e, 'ROC')
# plot(e, 'TPR')
# boxplot(e)
# density(e)





#-----------------------------------------------------------------
# Boosted Regression Tree - Predict (Bootstrap approach)
#-----------------------------------------------------------------
# For each habitat selection model (i.e., each life-history stage of each species), we fitted the model 50 times.
# For each of the 50 iterations, we used the parameter values chosen for the final model, but we sampled
# half the data (with replacement) to fit the model. (Hindell et al. 2020).
#
# Confidence intervals were calculated across 10 boosted regression tree
# model fits to account for model stochasticity (Hazen et al. 2018) 
#
# preserve 1/0 and stratify across individuals
#
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2011.00172.x

# import full dataset
# indir <- paste0(output_data, "/tracking/", sp_code, "/PresAbs/")
# obs_file <- paste0(indir, sp_code, "_observations.csv")
# data <- read.csv(obs_file)
# Transform skewed variables
# data$EKE <- log1p(data$EKE)
# data$CHL <- log1p(data$CHL)


# Set output directory
# Each bootstrap model is stored here
outdir_bootstrap <- paste0(outdir, "/bootstrap/")
if (!dir.exists(outdir_bootstrap)) dir.create(outdir_bootstrap, recursive = TRUE)


# Define number of bootstrap models
n.boot <- 50  # number of model fits

# Get hyper-parameters from full model
# Keep CV parameters
# mod_full_out <- data.frame(
#   tree.complexity = mod_full$interaction.depth,
#   learning.rate = mod_full$shrinkage,
#   bag.fraction = mod_full$bag.fraction,
#   n.trees = mod_full$n.trees
# ) 

## Prepare clusters
cores <- 50
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i=1:n.boot, .packages=c("dismo", "gbm", "dplyr", "splitstackshape", "stringr")) %dopar% {

  # sampled half the data (with replacement) to fit the model (Hindell et al. 2020)
  idata <- stratified(train, c("occ", "date"), 0.5, replace = TRUE)
  
  # fit BRT
  mod_boot <- dismo::gbm.fixed(data = idata,             # data.frame with data
                       gbm.x = pred_list,          # predictor variables
                       gbm.y = "occ",            # response variable
                       family = "bernoulli",  # the nature of errror structure
                       tree.complexity = tc,   # tree complexity
                       learning.rate = lr,  # learning rate
                       bag.fraction = bf,    # bag fraction
                       n.trees = ntrees) 
  
  # store model
  outfile <- paste0(outdir_bootstrap, "/", str_pad(i, 2, pad = "0"), "_", sp_code, "_", mod_code, "_boot.rds")
  saveRDS(mod_boot, file = outfile)  # save model
}

## stop clusters
stopCluster(cl)



#-----------------------------------------------------------------
# BRT - Predict on testing dataset
#-----------------------------------------------------------------
# for each in number of boots
# import model
# predict on testing dataset
# store
# then, we will average
# use average for cross-validation
# for spatial data, we should implement a similar approach of bootstrap: for each day, import habitat stack, then make n predictions, averange and calculate CI 


# boots_files <- list.files(outdir_bootstrap, full.names = T)
# 
# ## Prepare clusters
# cores <- length(boots_files)
# cl <- makeCluster(cores)
# registerDoParallel(cl)
# 
# preds <- foreach(i=1:n.boot, .packages=c("dismo", "gbm", "dplyr", "stringr")) %dopar% {
#   
#   # import model
#   ibrt <- readRDS(boots_files[i])
#   
#   # predict
#   preds <- predict.gbm(ibrt, test, n.trees=ibrt$gbm.call$best.trees, type="response")
#   list(bpred = preds)
# }
# 
# # average preds
# bpreds <- bind_cols(foreach(i=1:n.boot) %dopar% preds[[i]]$bpred)
# bpreds$mean <- rowMeans(bpreds)
# 
# # calculate deviance
# calc.deviance(obs=test$occ, pred=bpreds$mean, calc.mean=TRUE)
# 
# d <- cbind(test$occ, bpreds$mean)
# pres <- d[d[,1]==1, 2]
# abs <- d[d[,1]==0, 2]
# e <- evaluate(p=pres, a=abs)
# 
# plot(e, 'ROC')
# plot(e, 'TPR')
# boxplot(e)
# density(e)
# 
# ## stop clusters
# stopCluster(cl)

