#---------------------------------------------------------------------------------------------------
# fit_brt             Fit Species distribution model
#---------------------------------------------------------------------------------------------------
# create a grid of hyper-parameters (learning rate, depth, min obs in nodes, etc...)
# and build a full model on each combination of parameters in parallel. 

# Model optiomization prioritized:
# models with larger learning rates, smaller tree complexities and fewer trees to reduce overfitting (Elith et al. 2008)
# ensure that at least 1,000 trees were included in the final model configuration (Elith et al., 2008).                                                                                                     


mod_code <- "brt"

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste0(output_data, "/habitat-model-v2/", sp_code, "/")
outdir <- paste(output_data, "habitat-model-v2", sp_code, mod_code, sep="/")
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


#-----------------------------------------------------------------
# Prepare folds
#-----------------------------------------------------------------
# leave-one-out strategy
n.folds <- 14


set.seed(123)
train$id <- as.factor(train$id)
f <- fold(data = train, id_col = "id", method = "n_dist", k = n.folds)

# prepare
train <- f %>%
  dplyr::rename(fold = .folds) %>%
  dplyr::mutate(fold = as.numeric(fold)) %>%
  as.data.frame()


table(train$fold)
train %>%
  group_by(id, fold) %>%
  dplyr::summarize(n = n())

#-----------------------------------------------------------------
# Boosted Regression Tree - Optimization of hyper-parameters
#-----------------------------------------------------------------

# Add the RN to the list of variables
vars <- c(vars, "RN")

# Define number of trees
ini.nt = 50
max.nt = 10000
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
  # Uses a block cross-validation
  # faster learning rate means larger values
  mod <- tryCatch(
                  dismo::gbm.step(data = train,             # data.frame with data
                  gbm.x = vars,          # predictor variables
                  gbm.y = "occ",            # response variable
                  family = "bernoulli",  # the nature of errror structure
                  tree.complexity = comb$tc[i],   # tree complexity
                  learning.rate = comb$lr[i],  # learning rate
                  bag.fraction = comb$bf[i],    # bag fraction
                  fold.vector = train$fold,
                  n.folds = length(unique(train$fold)),
                  n.trees = ini.nt, step.size = step.nt, max.trees = max.nt)   
                  , error = function(e) return(NULL))
  
  
  if(!is.null(mod)) {
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
  scale_x_continuous(limits = c(0, max(cv_deviance$ntrees[!is.na(cv_deviance$cv_deviance)]))) +
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

select_model_id <- 11 # selection is based on parameters, criteria and checking curves

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

dismo::gbm.perspec(mod_full, 5, 9)
gbm.perspec(mod_full, 12, 9)
gbm.perspec(mod_full, 12, 10)
dismo::gbm.perspec(mod_full, 14, 12)



#-----------------------------------------------------------------
# Boosted Regression Tree - Predict (Bootstrap approach)
#-----------------------------------------------------------------
# For each habitat selection model (i.e., each life-history stage of each species), we fitted the model 50 times.
# For each of the 50 iterations, we used the parameter values chosen for the final model, but we sampled
# half the data (with replacement) to fit the model. (Hindell et al. 2020).


# Set output directory
# Each bootstrap model is stored here
outdir_bootstrap <- paste0(outdir, "/bootstrap/")
if (!dir.exists(outdir_bootstrap)) dir.create(outdir_bootstrap, recursive = TRUE)


# Define number of bootstrap models
n.boot <- 50  # number of model fits

## Prepare clusters
cores <- 50
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i=1:n.boot, .packages=c("dismo", "gbm", "dplyr", "splitstackshape", "stringr")) %dopar% {

  # sampled half the data (with replacement) to fit the model (Hindell et al. 2020)
  idata <- stratified(train, c("occ", "id", "date"), 0.5, replace = TRUE)
  
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
