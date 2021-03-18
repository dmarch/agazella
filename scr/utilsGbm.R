##################################################################################################
# Title: utilsGbm.R
# Abstract: Custom functions for BRT models and model evaluation
# Last update: 2014-4-8
# Author: David March
# Email: damamo82@gmail.com
#
# LIST OF FUNCTIONS
# dev.expl            Percentage of deviance explained (D^2)
# gbm.boot            Boostrap BRT with offset
# gbm.fixed.offset    gbm fixed with offset
# gbm.optim           Generate multiple combinations of parameters in gbm
# gbm.persp.col       Plot interactions from BRT models using coloured surfaces
# model.diag          Diagnostics for binomial and logistic model predictions
# model.performance   Combine metrics of predictive ability and spatial autocorrelation
# plot.gbm.optim      Plot gbm.optim objects
# predict.boot        Boostrap BRT with offset
# predict.gbm.mix     DEPRECATED :: Predict single or mixture models with offset
# predict.gbm.offset  Predict gbm models with an offset term
# rac.sp              Spatial Residual Autocovariate
# rac.sptp            Spatio-Temporal Residual Autocovariate
# reff.fold           Create folds for random effect structures
# resid.autocor       Calculate Moran's I for a set of surveys
##################################################################################################
# 
# Notes:
# revise resid.autocor & model.performance: consider more than one survey per day


#----------------------------------------------------------------------------------
# dev.expl      Percentage of deviance explained (D^2)
#----------------------------------------------------------------------------------
# Most part of the code is extracted from 'gbm.fixed' function from the dismo package
# used in model.diag
dev.expl<-function(obs,pred,family,site.weights = rep(1, length(y_i))){
  
  y.data<-obs
  fitted.values<-pred
  
  y_i <- y.data
  u_i <- fitted.values
  if (family == "poisson") {
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - 
      (y_i - u_i)
    resid.deviance <- 2 * sum(deviance.contribs * site.weights)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
    u_i <- sum(y.data * site.weights)/sum(site.weights)
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - 
      (y_i - u_i)
    total.deviance <- 2 * sum(deviance.contribs * site.weights)
  }
  if (family == "bernoulli") {
    deviance.contribs <- (y_i * log(u_i)) + ((1 - y_i) * 
      log(1 - u_i))
    resid.deviance <- -2 * sum(deviance.contribs * site.weights)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
    u_i <- sum(y.data * site.weights)/sum(site.weights)
    deviance.contribs <- (y_i * log(u_i)) + ((1 - y_i) * 
      log(1 - u_i))
    total.deviance <- -2 * sum(deviance.contribs * site.weights)
  }
  if (family == "laplace") {
    resid.deviance <- sum(abs(y_i - u_i))
    residuals <- y_i - u_i
    u_i <- mean(y.data)
    total.deviance <- sum(abs(y_i - u_i))
  }
  if (family == "gaussian") {
    resid.deviance <- sum((y_i - u_i) * (y_i - u_i))
    residuals <- y_i - u_i
    u_i <- mean(y.data)
    total.deviance <- sum((y_i - u_i) * (y_i - u_i))
  }
  
  deviance.explained <- ((total.deviance-resid.deviance)/total.deviance)*100
  
  return (deviance.explained)
}
#----------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------
# gbm.boot          Boostrap BRT with offset
#-------------------------------------------------------------------------------------------------
gbm.boot<-function(n.boot,group,data,gbm.x,gbm.y,family,learning.rate,tree.complexity,bag.fraction,n.trees,offset,parallel,folder=NULL,keep.data=FALSE){
  require(plyr)
  if (n.boot <= 0) stop ("no boostrap elements")
  
  ########################
  # Resample and fit BRT 
  boot.brt<-function(i,data,group,gbm.x,gbm.y,family,learning.rate,tree.complexity,bag.fraction,n.trees,offset,partition=1,folder=NULL,keep.data=FALSE){
    require(dismo)
    source("utilsGbm.R")    
    
    # resample original data with replacement using groups (uses a 70%)
    un<-unique(data[,group])
    gg <- un[sample.int(length(un)*partition, replace=TRUE)]
    gg<-data.frame(gg)
    names(gg)<-names(data)[group]
    dd<-merge(gg,data,by=names(data)[group]) #merge retains duplicated surveys
    
    #redefine offset
    off<-dd[,offset]
    
    #fit model on resampled dataset
    mb<-gbm.fixed.offset(data=dd,
                         gbm.x=gbm.x,                #column predictor variables
                         gbm.y=gbm.y,                    #column response variable
                         offset=off,  #=NULL if applicable
                         family=family,         #bernoulli for presence/absence data
                         tree.complexity=tree.complexity,          #number of nodes in the tree
                         learning.rate=learning.rate,         #slow this down to ensure at least 1000 tree are fitted
                         n.trees=n.trees,
                         bag.fraction=bag.fraction,
                         keep.data = keep.data)
    
    #save fit or return
    if (!is.null(folder)){
      ifile<-paste(folder,paste("boot",family,format(Sys.time(),"%Y%m%d%H%M%S"),i,sep="_"),".RData",sep="")
      save(mb,file=ifile)
    } else {    
      return(mb)
    }
  }
  
  #########################
  # Bootstraps
  
  #parallel <- FALSE
  if ( parallel ) {
    # parallelise bootstraps
    suppressPackageStartupMessages(library("parallel", quietly=TRUE))
    n <- 10#detectCores() #8
    
    # parallel
    cl <- makeCluster(n)
    boot <- clusterApply(cl=cl, x=1:n.boot, fun=boot.brt, 
                         data=data,
                         gbm.x=gbm.x,                #column predictor variables
                         gbm.y=gbm.y,                    #column response variable
                         offset=offset,  #=NULL if applicable
                         family=family,         #bernoulli for presence/absence data
                         tree.complexity=tree.complexity,          #number of nodes in the tree
                         learning.rate=learning.rate,         #slow this down to ensure at least 1000 tree are fitted
                         n.trees=n.trees,
                         bag.fraction=bag.fraction,
                         group=group,
                         folder=folder,
                         keep.data=keep.data)
    stopCluster(cl)
    
  } else {
    boot <- alply(1:n.boot, 1, .fun=boot.brt,
                  data=data,
                  gbm.x=gbm.x,                #column predictor variables
                  gbm.y=gbm.y,                    #column response variable
                  offset=offset,  #=NULL if applicable
                  family=family,         #bernoulli for presence/absence data
                  tree.complexity=tree.complexity,          #number of nodes in the tree
                  learning.rate=learning.rate,         #slow this down to ensure at least 1000 tree are fitted
                  n.trees=n.trees,
                  bag.fraction=bag.fraction,
                  group=group,
                  folder=folder,
                  keep.data=keep.data,
                  .progress="text")
  }
  
  return(boot)
}
#-------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# gbm.fixed.offset       gbm fixed with offset
#--------------------------------------------------------------------------------------------
# Comments:
# This function extends 'gbm.fixed' by allowing the inclussion of an offset term. The modifications
# for incorporating 'offset' are commented. They are based on the code from 'gbm.step'.
# For model prediction, it uses the new 'predict.gbm.offset' function.
# Binomial models are not supported.
#
# Arguments:
# See gbm.fixed
gbm.fixed.offset<-function (data, gbm.x, gbm.y, offset = NULL, tree.complexity = 1,
                            site.weights = rep(1,nrow(data)), verbose = TRUE, learning.rate = 0.001, n.trees = 2000, 
                            bag.fraction = 0.5, family = "poisson", keep.data = FALSE, 
                            var.monotone = rep(0, length(gbm.x))) 
{
  train.fraction = 1
  if (!require(gbm)) {
    stop("you need to install the gbm package to run this function")
  }
  #if (family == "bernoulli") {
  #  stop("Offset terms in binomial models are not supported")
  #}
  dataframe.name <- deparse(substitute(data))
  x.data <- eval(data[, gbm.x])
  names(x.data) <- names(data)[gbm.x]
  y.data <- eval(data[, gbm.y])
  sp.name <- names(data)[gbm.y]
  z1 <- unclass(Sys.time())
  
  ###### offset #####
  offset.name <- deparse(substitute(offset))
  offset = eval(offset)
  if (is.null(offset)) {
    gbm.call <- paste("gbm(y.data ~ .,n.trees = n.trees, data=x.data, verbose = F, interaction.depth = tree.complexity, \n    weights = site.weights, shrinkage = learning.rate, distribution = as.character(family),\n    var.monotone = var.monotone, bag.fraction = bag.fraction, keep.data = keep.data)", 
                      sep = "")
  }
  else {
    gbm.call <- paste("gbm(y.data ~ . + offset(offset), \n     n.trees = n.trees, data=x.data, verbose = F, interaction.depth = tree.complexity, \n    weights = site.weights, shrinkage = learning.rate, distribution = as.character(family),\n    var.monotone = var.monotone, bag.fraction = bag.fraction, keep.data = keep.data)", 
                      sep = "")
  }
  ###################
  
  if (verbose) {
    print(paste("fitting gbm model with a fixed number of ", 
                n.trees, " trees for ", sp.name, sep = ""), quote = FALSE)
  }
  gbm.object <- eval(parse(text = gbm.call))
  best.trees <- n.trees
  
  ###### offset #####
  fitted.values <- predict.gbm.offset(gbm.object, x.data, n.trees = n.trees,offset) 
  ###### offset #####
  
  gbm.summary <- summary(gbm.object, n.trees = n.trees, plotit = FALSE)
  y_i <- y.data
  u_i <- fitted.values
  if (family == "poisson") {
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - 
      (y_i - u_i)
    resid.deviance <- 2 * sum(deviance.contribs * site.weights)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
    u_i <- sum(y.data * site.weights)/sum(site.weights)
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - 
      (y_i - u_i)
    total.deviance <- 2 * sum(deviance.contribs * site.weights)
  }
  if (family == "bernoulli") {
    deviance.contribs <- (y_i * log(u_i)) + ((1 - y_i) * 
                                               log(1 - u_i))
    resid.deviance <- -2 * sum(deviance.contribs * site.weights)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
    u_i <- sum(y.data * site.weights)/sum(site.weights)
    deviance.contribs <- (y_i * log(u_i)) + ((1 - y_i) * 
                                               log(1 - u_i))
    total.deviance <- -2 * sum(deviance.contribs * site.weights)
  }
  if (family == "laplace") {
    resid.deviance <- sum(abs(y_i - u_i))
    residuals <- y_i - u_i
    u_i <- mean(y.data)
    total.deviance <- sum(abs(y_i - u_i))
  }
  if (family == "gaussian") {
    resid.deviance <- sum((y_i - u_i) * (y_i - u_i))
    residuals <- y_i - u_i
    u_i <- mean(y.data)
    total.deviance <- sum((y_i - u_i) * (y_i - u_i))
  }
  if (verbose) {
    print(paste("total deviance = ", round(total.deviance, 
                                           2), sep = ""), quote = F)
    print(paste("residual deviance = ", round(resid.deviance, 
                                              2), sep = ""), quote = F)
  }
  z2 <- unclass(Sys.time())
  elapsed.time.minutes <- round((z2 - z1)/60, 2)
  gbm.detail <- list(dataframe = dataframe.name, gbm.x = gbm.x, 
                     predictor.names = names(x.data), gbm.y = gbm.y, reponse.name = names(y.data), offset = offset.name, ###### offset #####
                     family = family, tree.complexity = tree.complexity, learning.rate = learning.rate, 
                     bag.fraction = bag.fraction, cv.folds = 0, n.trees = n.trees, 
                     best.trees = best.trees, train.fraction = train.fraction, 
                     var.monotone = var.monotone, date = date(), elapsed.time.minutes = elapsed.time.minutes)
  gbm.object$gbm.call <- gbm.detail
  gbm.object$fitted <- fitted.values
  gbm.object$residuals <- residuals
  gbm.object$contributions <- gbm.summary
  gbm.object$self.statistics <- list(null.deviance = total.deviance, 
                                     resid.deviance = resid.deviance)
  gbm.object$weights <- site.weights
  return(gbm.object)
}
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# gbm.optim       Generate multiple combinations of parameters in gbm
#--------------------------------------------------------------------------------------------
# Arguments:
# data    input data.frame 
# gbm.x   predictor variables
# gbm.y   response variable
# offset  variable for offset (without log)
# family  family
# lr      vector of learning rate values
# tc      vector of tree complexity values
# bf      vector of bag.fraction values
# init.nt initial number of trees
# max.nt  maximum number of trees
# step.nt step of vector of trees
# test    testing dataset
# test.y  response variable in testing dataset
# tolerance set tolerance for the minimum difference to consider minimum deviance.
#
# Note:
# Based on ideas from Elith et al. 2008, Gregory et al. 2012
#
# Example:
# See 7_BRT.R

gbm.optim<-function(data,gbm.x,gbm.y,offset,family,lr,tc,bf,ini.nt,max.nt,step.nt,test,test.y,tolerance = 0.001){
  
  #create outputs: combination and pred.deviance
  comb=expand.grid(lr=lr,tc=tc,bf=bf,comp.time=0,deviance=0,min.ntrees=0) #combination
  tree.list=seq(ini.nt,max.nt,by=step.nt) #list of trees for evaluation
  pred.deviance <- matrix(data=NA,nrow=length(tree.list),ncol=nrow(comb))#rep(0,100) #matrix of ntrees.steps vs combinations
  
  # Run loop for each combination
  for (i in 1:nrow(comb)){
    print(paste("Combination",i,"of",nrow(comb),sep=" "))
    
    # Fit all combinations
    m0<-gbm.fixed(data=data,
                         gbm.x=gbm.x, #column predictor variables
                         gbm.y=gbm.y, #column response variable
                         family=family, #bernoulli for presence/absence data
                         tree.complexity=comb$tc[i], 
                         learning.rate=comb$lr[i],
                         n.trees=max.nt,
                         bag.fraction=comb$bf[i],
                         keep.data=TRUE,
                         verbose=FALSE)
    comb$comp.time[i]=m0$gbm.call$elapsed.time.minutes #computing time (in minutes)
    
    # Predict model on testing dataset at multiples tree steps
    pred<-predict.gbm.offset(model=m0,newdata=test,n.trees=tree.list)
    
    # Calculate deviance for each tree step
    for (j in 1:dim(pred)[2]) {
      pred.deviance[j,i] <- calc.deviance(test.y, pred[,j],calc.mean=T,family=family)
    }
    
    #identify minimum deviances for each combination ntrees
    n.cases <- nrow(data)
    total.deviance<-calc.deviance(test.y, pred[,j],calc.mean=FALSE,family=family)
    mean.total.deviance <- total.deviance/n.cases
    tolerance.test <- mean.total.deviance * tolerance
    loss.values<-pred.deviance[,i]
    
    for (z in 10:length(loss.values)){
      test1 <- mean(loss.values[(z - 4):z])
      test2 <- mean(loss.values[(z - 9):(z - 5)])
      delta.deviance <- test2 - test1
      if (delta.deviance < tolerance.test) break
    }
    
    comb$deviance[i]<-pred.deviance[z,i]
    comb$min.ntrees[i]<-ini.nt+step.nt*(which(pred.deviance[,i]==comb$deviance[i])-1)
  } #end loop
  
  return(list(comb=comb, pred.deviance=pred.deviance, tree.list=tree.list, sample.size=nrow(data)))
}#end function
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# gbm.persp.col     Plot interactions from BRT models using coloured surfaces
#--------------------------------------------------------------------------------------------
# This is just a slight modification of gbm.perspec from dismo package to allow coloured surface

gbm.perspec.col<-function (gbm.object, x = 1, y = 2, pred.means = NULL, x.label = NULL, 
                           x.range = NULL, y.label = NULL, z.label = "fitted value", 
                           y.range = NULL, z.range = NULL, leg.coords = NULL, ticktype = "detailed", 
                           theta = 55, phi = 40, smooth = "none", mask = FALSE, perspective = TRUE,
                           col = "white", #included in this new function
                           ...) 
{
  if (!require(gbm)) {
    stop("you need to install the gbm package to use this function")
  }
  if (!require(splines)) {
    stop("you need to install the splines package to use this function")
  }
  gbm.call <- gbm.object$gbm.call
  gbm.x <- gbm.call$gbm.x
  n.preds <- length(gbm.x)
  gbm.y <- gbm.call$gbm.y
  pred.names <- gbm.call$predictor.names
  family = gbm.call$family
  have.factor <- FALSE
  x.name <- gbm.call$predictor.names[x]
  if (is.null(x.label)) {
    x.label <- gbm.call$predictor.names[x]
  }
  y.name <- gbm.call$predictor.names[y]
  if (is.null(y.label)) {
    y.label <- gbm.call$predictor.names[y]
  }
  data <- eval(parse(text = gbm.call$dataframe))[, gbm.x]
  n.trees <- gbm.call$best.trees
  if (is.vector(data[, x])) {
    if (is.null(x.range)) {
      x.var <- seq(min(data[, x], na.rm = T), max(data[, 
                                                       x], na.rm = T), length = 50)
    }
    else {
      x.var <- seq(x.range[1], x.range[2], length = 50)
    }
  }
  else {
    x.var <- names(table(data[, x]))
    have.factor <- TRUE
  }
  if (is.vector(data[, y])) {
    if (is.null(y.range)) {
      y.var <- seq(min(data[, y], na.rm = T), max(data[, 
                                                       y], na.rm = T), length = 50)
    }
    else {
      y.var <- seq(y.range[1], y.range[2], length = 50)
    }
  }
  else {
    y.var <- names(table(data[, y]))
    if (have.factor) {
      stop("at least one marginal predictor must be a vector!")
    }
    else {
      have.factor <- TRUE
    }
  }
  pred.frame <- expand.grid(list(x.var, y.var))
  names(pred.frame) <- c(x.name, y.name)
  pred.rows <- nrow(pred.frame)
  if (have.factor) {
    if (is.factor(pred.frame[, 2])) {
      pred.frame <- pred.frame[, c(2, 1)]
      x.var <- y.var
    }
  }
  j <- 3
  for (i in 1:n.preds) {
    if (i != x & i != y) {
      if (is.vector(data[, i])) {
        m <- match(pred.names[i], names(pred.means))
        if (is.na(m)) {
          pred.frame[, j] <- mean(data[, i], na.rm = T)
        }
        else pred.frame[, j] <- pred.means[m]
      }
      if (is.factor(data[, i])) {
        m <- match(pred.names[i], names(pred.means))
        temp.table <- table(data[, i])
        if (is.na(m)) {
          pred.frame[, j] <- rep(names(temp.table)[2], 
                                 pred.rows)
        }
        else {
          pred.frame[, j] <- pred.means[m]
        }
        pred.frame[, j] <- factor(pred.frame[, j], levels = names(temp.table))
      }
      names(pred.frame)[j] <- pred.names[i]
      j <- j + 1
    }
  }
  prediction <- predict.gbm(gbm.object, pred.frame, n.trees = n.trees, 
                            type = "response")
  if (smooth == "model") {
    pred.glm <- glm(prediction ~ ns(pred.frame[, 1], df = 8) * 
                      ns(pred.frame[, 2], df = 8), data = pred.frame, family = poisson)
    prediction <- fitted(pred.glm)
  }
  max.pred <- max(prediction)
  cat("maximum value = ", round(max.pred, 2), "\n")
  if (is.null(z.range)) {
    if (family == "bernoulli") {
      z.range <- c(0, 1)
    }
    else if (family == "poisson") {
      z.range <- c(0, max.pred * 1.1)
    }
    else {
      z.min <- min(data[, y], na.rm = T)
      z.max <- max(data[, y], na.rm = T)
      z.delta <- z.max - z.min
      z.range <- c(z.min - (1.1 * z.delta), z.max + (1.1 * 
                                                       z.delta))
    }
  }
  if (have.factor == FALSE) {
    pred.matrix <- matrix(prediction, ncol = 50, nrow = 50)
    if (smooth == "average") {
      pred.matrix.smooth <- pred.matrix
      for (i in 2:49) {
        for (j in 2:49) {
          pred.matrix.smooth[i, j] <- mean(pred.matrix[c((i - 
                                                            1):(i + 1)), c((j - 1):(j + 1))])
        }
      }
      pred.matrix <- pred.matrix.smooth
    }
    if (mask) {
      mask.trees <- gbm.object$gbm.call$best.trees
      point.prob <- predict.gbm(gbm.object[[1]], pred.frame, 
                                n.trees = mask.trees, type = "response")
      point.prob <- matrix(point.prob, ncol = 50, nrow = 50)
      pred.matrix[point.prob < 0.5] <- 0
    }
    if (!perspective) {
      image(x = x.var, y = y.var, z = pred.matrix, zlim = z.range)
    }
    else {
      
      #inset this code in gbm.persp to allow surface colours corresponding to z-values
      if (length(col)>1) {
        color <- col
        z<-pred.matrix
        nrz <- nrow(z)
        ncz <- ncol(z)
        zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
        facetcol <- cut(zfacet, length(col))
        col<-color[facetcol]
      }
      #end of modfication
      
      persp(x = x.var, y = y.var, z = pred.matrix, zlim = z.range, 
            xlab = x.label, ylab = y.label, zlab = z.label, 
            theta = theta, phi = phi, r = sqrt(10), d = 3, 
            ticktype = ticktype, mgp = c(4, 1, 0), 
            col=col, #included in this new function
            ...)
    }
  }
  if (have.factor) {
    factor.list <- names(table(pred.frame[, 1]))
    n <- 1
    if (is.null(z.range)) {
      vert.limits <- c(0, max.pred * 1.1)
    }
    else {
      vert.limits <- z.range
    }
    plot(pred.frame[pred.frame[, 1] == factor.list[1], 2], 
         prediction[pred.frame[, 1] == factor.list[1]], type = "l", 
         ylim = vert.limits, xlab = y.label, ylab = z.label, 
         ...)
    for (i in 2:length(factor.list)) {
      factor.level <- factor.list[i]
      lines(pred.frame[pred.frame[, 1] == factor.level, 
                       2], prediction[pred.frame[, 1] == factor.level], 
            lty = i)
    }
    if (is.null(leg.coords)) {
      x.max <- max(pred.frame[, 2])
      x.min <- min(pred.frame[, 2])
      x.range <- x.max - x.min
      x.pos <- c(x.min + (0.02 * x.range), x.min + (0.3 * 
                                                      x.range))
      y.max <- max(prediction)
      y.min <- min(prediction)
      y.range <- y.max - y.min
      y.pos <- c(y.min + (0.8 * y.range), y.min + (0.95 * 
                                                     y.range))
      legend(x = x.pos, y = y.pos, factor.list, lty = c(1:length(factor.list)), 
             bty = "n")
    }
    else {
      legend(x = leg.coords[1], y = leg.coords[2], factor.list, 
             lty = c(1:length(factor.list)), bty = "n")
    }
  }
}
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# model.diag      Diagnostics for binomial and logistic model predictions
#--------------------------------------------------------------------------------------------
model.diag<-function (obs,pred,family,spi=NULL,tempi=NULL,agreg=NA, calendarmap=FALSE, map=FALSE) {
  # Description:
  # Calculate metrics for comparing observed and predicted values
  #
  # Arguments:
  #-obs           observed values
  #-pred          predicted values
  #-family        model family. currently supports "binomial" and "poisson"
  #-agreg         aggregate by sptial or temporal index
  #-spi           spatial index
  #-tempi         temporal index
  #-calendarmap   no yet implemented
  #-map           no yet implemented
  #
  # Author:
  # David March damamo82@gmail.com
  #
  # Todo: aggregations may be done before usign this function. It will reduce: tempi, spi, level... Only once has to be done.
  # last version: included explained.deviance.
  
  require(PresenceAbsence)
  require(lmodel2)
  source("plots.R")# custom functions distributed by Phillips, S. J., and J. Elith. 2010. POC plots: calibrating species distribution models with presence-only data. Ecology 91:2476-2484
  
  #diagnostics for binomial data:
  if (family=="bernoulli"){
    #evaluation
    deviance<-calc.deviance(obs=obs,pred=pred,family=family,calc.mean=TRUE)
    auc<-auc(data.frame(id=seq(1:length(obs)),obs,pred),which.model=1,st.dev=FALSE)
    
    #calibration
    cor<-cor(pred,obs)
    w<-ecalp(pred, obs)
    w<-as.data.frame(w)
    w$bin<-c(0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95)
    caltest<-lm(w~bin, data=w, subset=w>-1)
    calibration<-caltest$coefficients[2][[1]]
    bias<-caltest$coefficients[1][[1]]
    explained.deviance=dev.expl(obs,pred,family)
    
    #return results
    return(data.frame(family=family,deviance=deviance, auc=auc, cor=cor, calibration=calibration, bias=bias, explained.deviance=explained.deviance))
  }
  
  #diagnostics for count data
  if (family=="poisson" | family=="gaussian"){
    
    #calculate metrics
    deviance<-calc.deviance(obs=obs,pred=pred,family=family,calc.mean=TRUE)
    pearson<-cor(obs,pred,method="pearson")
    spearman<-cor(obs,pred,method="spearman")
    #caltest<-lmodel2(log(obs+0.000001)~log(pred), nperm=100)      ### regression is performed on log-transformed data, requiring a small addition to avoid -inf when data=0 
    caltest<-lmodel2(obs~pred, nperm=100)     
    R_sq<-caltest$rsquare
    #calibration<-caltest$regression.results[2,3]
    #bias<-caltest$regression.results$Intercept[2]
    calibration<-caltest$regression.results[1,3]
    bias<-caltest$regression.results$Intercept[1]
    rmse<-sqrt(mean((pred-obs)^2))
    explained.deviance=dev.expl(obs,pred,family)
    
    #return results
    return(data.frame(family=family,deviance=deviance, r=pearson, rho=spearman, R_sq=R_sq, calibration=calibration, bias=bias,rmse=rmse,explained.deviance=explained.deviance))
  }
}
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# model.performance   Combine metrics of predictive ability and spatial autocorrelation
#--------------------------------------------------------------------------------------------
model.performance<-function(obs,pred,x=NULL,y=NULL,date=NULL,family){
  
  results<-NULL
  # consider multiple predictions from different models
  nc<-ncol(pred)
  for (i in 1:ncol(pred)){
  print(paste("Processing model",i,"from",ncol(pred),sep=" "))
    
  # predictive ability
  print("Calculating predictive ability...")
  diag=model.diag(obs,pred[,i],family)
  df=diag
  df=data.frame(Model=names(pred)[i],diag)
  
  # spatial autcorrelation
  print("Calculating spatial autocorrelation...")
  if (!is.null(x) & !is.null(y) & !is.null(date)) {
    spres<-resid.autocor(obs,pred[,i],x,y,date,family)
    MoranI<-data.frame(MoranI.mean=mean(spres$MoranI),MoranI.sd=sd(spres$MoranI))
    df<-cbind(df,MoranI)
  }
  
  results<-rbind(results,df)
  }#end for i
  return(results)
}
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# plot.gbm.optim      Plot gbm.optim objects
#--------------------------------------------------------------------------------------------
# Description:
# allow the exploration of multiple combinations of parameters to aid parameter selection
#
# Arguments:
#-optim     object generated with function gbm.optim
#-type      type of plot. Possible: "surf.deviance","surf.trees","deviance","trees","computer.time","learning.rate"
#-tc        If type="learning.rate" this is the number of tree complexity used for the plot
#-bf        If type="learning.rate" this is the bag.fraction used for the plot
#-max.trees If type="learning.rate" this is the maximum number of trees used for the plot
#
# Author:
# David March damamo82@gmail.com 
#
# Note:
# Based on ideas from Elith et al. 2008, Gregory et al. 2012
#
# Example:
# See 7_BRT.R

#todo:
#- exclude combinations where the minimum was not reached by max.ntrees
#- adjust yrange in plot "learning.rate" using max.value for such combination.
#- plot abline with minimum deviance value in plot "learning.rate".

plot.gbm.optim<-function(optim,type,tc,bf,max.trees){

  comb=optim$comb
  pred.deviance=optim$pred.deviance
  tree.list=optim$tree.list
  
  if(type=="surf.deviance"){
    m.dev<-glm(deviance~lr*tc,data=comb)
    tc=seq(min(comb$tc),max(comb$tc),1)
    lr=seq(min(comb$lr),max(comb$lr),0.001)
    f.dev=function(tc,lr){predict(m.dev,newdata=data.frame(lr,tc))}
    z<-outer(tc, lr, f.dev)
    image.plot(tc,lr,z,col=tim.colors(),ylab="lr",main="Deviance")
  }
  if(type=="surf.trees"){
    m.trees<-glm(min.ntrees~lr*tc,data=comb)
    tc=seq(min(comb$tc),max(comb$tc),1)
    lr=seq(min(comb$lr),max(comb$lr),0.001)
    f.trees=function(tc,lr){predict(m.trees,newdata=data.frame(lr,tc))}
    z<-outer(tc, lr, f.trees)
    image.plot(tc,lr,z,col=tim.colors(),ylab="lr",main="Min ntrees")
  }
  if(type=="deviance"){
    comb$tc<-as.factor(comb$tc)
    comb$lr<-as.factor(comb$lr)
    comb$bf<-as.factor(comb$bf)
    op <- par(mfrow = c(1, 3))
    plot(comb$tc,comb$deviance,xlab="Tree complexity",ylab="Deviance")
    plot(comb$lr,comb$deviance,xlab="Learning rate",ylab="Deviance")
    plot(comb$bf,comb$deviance,xlab="Bag fraction",ylab="Deviance")
    par(op)
  }  
  if(type=="trees"){
    comb$tc<-as.factor(comb$tc)
    comb$lr<-as.factor(comb$lr)
    comb$bf<-as.factor(comb$bf)
    op <- par(mfrow = c(1, 3))
    plot(comb$tc,comb$min.ntrees,xlab="Tree complexity",ylab="Min ntrees")
    plot(comb$lr,comb$min.ntrees,xlab="Learning rate",ylab="Min ntress")
    plot(comb$bf,comb$min.ntrees,xlab="Bag fraction",ylab="Min ntrees")
    par(op)
  }
  if(type=="computer.time"){
    comb$tc<-as.factor(comb$tc)
    comb$lr<-as.factor(comb$lr)
    comb$bf<-as.factor(comb$bf)
    op <- par(mfrow = c(1, 3))
    plot(comb$tc,comb$comp.time,xlab="Tree complexity",ylab="Computer time")
    plot(comb$lr,comb$comp.time,xlab="Learning rate",ylab="Computer time",main="computer time")
    plot(comb$bf,comb$comp.time,xlab="Bag fraction",ylab="Computer time")
    par(op)
  }
  if(type=="learning.rate"){
    #create subset of deviance matrix based on criteria
    sel<-which(comb$bf==bf & comb$tc==tc)
    sel.pred<-pred.deviance[,sel]
    lr.sel<-comb$lr[sel]
    # set up the plot
    xrange=c(0,max.trees)
    yrange=c(min(sel.pred),max(sel.pred))
    nlines=length(sel)
    
    plot(xrange, yrange, type="n", xlab="Number of trees",
         ylab="Predictive deviance" )
    colors <- rainbow(nlines)
    linetype <- c(1:nlines)
    
    # add lines
    for (i in 1:nlines) {
      lines(tree.list, sel.pred[,i], type="l", lwd=2,
            lty=linetype[i], col=colors[i])
    } 
    
    # add a title and subtitle
    title(paste("Tree complexity = ",tc," Bag fraction = ",bf))
    
    # add a legend
    legend("topright", legend=lr.sel, lty=linetype,col=colors,cex=0.8, title = "LR",inset=0.02)
  }
} #end function
#----------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------
# predict.boot          Boostrap BRT with offset
#-------------------------------------------------------------------------------------------------
predict.boot<-function(m.boot,newdata,offset){
  
  source("utilsGbm.R")
  
  #predict values for each boostrap
  pred<-laply(m.boot, function(mb,newdata,offset){
    predict.gbm.offset(mb,newdata=newdata,offset=offset)
  }, newdata=newdata,offset=offset)
  
  return(pred)
}
#-------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
# predict.gbm.offset   Predict gbm models with an offset term
#--------------------------------------------------------------------------------------------
# Arguments:
# model      gbm model object
# newdata testing dataset
# offset  offset variable provided in log scale
# n.trees number of trees
#
# Example:
# See 7_BRT.R

predict.gbm.offset<-function (model,newdata,offset,n.trees=NULL){
  
  #set number of trees
  if (is.null(n.trees)){
    n.trees=model$gbm.call$best.trees
  } else {
    n.trees=n.trees #new
  } 
  
  #predict with link function
  model.fit<-predict.gbm(model,newdata,n.trees=n.trees,type="link")  
  
  #incorporate offset
  if (!is.null(offset)){
    model.fit<- model.fit+offset
  }
  if (model$distribution$name == "bernoulli") { #model$gbm.call$family
    model.fit <- exp(model.fit)/(1 + exp(model.fit))
    #stop("offset terms in binomial models are not supported")
  }
  if (model$distribution$name == "poisson") { #model$gbm.call$family
    model.fit <- exp(model.fit)    
  }
  pred<-model.fit
  return(pred)
}
#--------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------
# rac.sp      Spatial Residual Autocovariate
#----------------------------------------------------------------------------------
# Arguments:
# obs           observed values
# pred          predicted values
# family        model family. currently supports "bernoulli", "poisson" and "gaussian"
# x             easting coordinates
# y             northing coordinates
# group         vector for specification of groups
#
# Description:
# Calculate spatial autocovariate from residuals following (Crasse et al. 2012). The
# computation of residuals follows the code used in the gbm.step function from dismo package.
#
# Author:
# David March
#
# References:
# Crasse et al. 2012
# Elith
#
# Example:
# See 7_BRT.R

rac.sp<-function(obs,pred,family,x,y,date,group){
  
  #create dataframe to check that dimensions are equal
  df<-data.frame(obs,pred,x,y,group)
  
  #calculate residuals (code extracted from gbm.step function in dismo package)
  y_i <- df$obs
  u_i <- df$pred
  if (family == "bernoulli") {
    deviance.contribs <- (y_i * log(u_i)) + ((1 - y_i) * log(1 - u_i))
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
  }
  if (family == "poisson") {
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - (y_i - u_i)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
  }
  if (family == "gaussian" | family == "laplace") {
    residuals <- y_i - u_i
  }
  df$resid<-residuals
  
  #define steps
  un<-unique(group)
  df$ac<-NA
  
  ##################################################
  #calculate spatial autocovariate
  print("Calculating spatial autocovariate...")
  w <- matrix(c(1,1,1,1,0,1,1,1,1), nr=3,nc=3) #define window
  
  for (i in 1:length(un)){
    #subset data for survey i
    idata<-df[df$group==un[i],] #get data for survey i
    
    #new version for crasse
    coordinates(idata)=~x+y
    gridded(idata)<-TRUE
    irast<-raster(idata,layer="resid")
    
    #moving window
    ifocal <- focal(irast, w = w, fun = mean, pad=TRUE, na.rm = TRUE) #compute focal function
    
    #incorporate autocovariate into the dataset
    ac <- extract(ifocal, idata)
    df[df$group==un[i],"ac"]<-ac
  }
  
  return(df$ac)
}
#----------------------------------------------------------------------------------


#----------------------------------------------------------------------------------
# rac.sptp      Spatio-Temporal Residual Autocovariate
#----------------------------------------------------------------------------------
# Arguments:
# obs           observed values
# pred          predicted values
# family        model family. currently supports "bernoulli", "poisson" and "gaussian"
# x             easting coordinates
# y             northing coordinates
# date          date
# group         vector for specification of groups
# type          exponential factor
# t.before      days before the focal weight, to apply in the temporal window
# t.after       days after the focal weight, to apply in the temporal window
#
# Description:
# Extension from rac.sp to calculate the spatio-temporal autcovariate. This function averages
# the spatial autocovariate from past and future surveys using a moving window average.
# Opposite from rac.sp, when calculates the spatial autocovariate from other surveys it
# also takes into account the focal cell.
#
# Author:
# David March
#
# References:
# Crasse et al. 2012
# Elith
#
# Example:
# See 7_BRT.R

rac.sptp<-function(obs,pred,family,x,y,date,group=NULL,spi,t.before,t.after,type="one"){
  require(zoo)
  
  #create dataframe to check that dimensions are equal
  df<-data.frame(spi,obs,pred,x,y,date,group)
  
  #calculate residuals (code extracted from gbm.step function in dismo package)
  y_i <- df$obs
  u_i <- df$pred
  if (family == "bernoulli") {
    deviance.contribs <- (y_i * log(u_i)) + ((1 - y_i) * log(1 - u_i))
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
  }
  if (family == "poisson") {
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - (y_i - u_i)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
  }
  if (family == "gaussian" | family == "laplace") {
    residuals <- y_i - u_i
  }
  df$resid<-residuals
  
  #define steps
  if (!is.null(group)) un<-unique(group) else un<-unique(date)
  df$ac<-NA
  
  ##################################################
  #calculate spatial autocovariate
  print("Calculating spatial autocovariate...")
  
  #need to calculate the spatial autocovariate but including the focal cell!
  df$ac<-NA
  w <- matrix(c(1,1,1,1,1,1,1,1,1), nr=3,nc=3) #define window
  
  for (i in 1:length(un)){
    #subset data for survey i
    idata<-df[df$group==un[i],] #get data for survey i
    
    #new version for crasse
    coordinates(idata)=~x+y
    gridded(idata)<-TRUE
    irast<-raster(idata,layer="resid")
    
    #moving window
    ifocal <- focal(irast, w = w, fun = mean, pad=TRUE, na.rm = TRUE) #compute focal function
    
    #incorporate autocovariate into the dataset
    ac <- extract(ifocal, idata)
    df[df$group==un[i],"ac"]<-ac
  }
  
  ##################################################
  ###calculate temporal autocovariate
  print("Calculating temporal autocovariate...")
  
  # set exponential value
  if (type == "one") 
    expo <- 0
  if (type == "inverse") 
    expo <- 1
  if (type == "inverse.squared") 
    expo <- 2
  
  #subset by spatial index
  df$act<-NA
  un<-unique(spi)
  for (j in 1:length(un)){
    print(paste("Processing cell",j,"from",length(un),"cells",sep=" "))
    
    jdata<-df[df$spi==un[j],c("ac","date")]
    jzoo<-zoo(jdata$ac,jdata$date)
    
    for (t in 1:length(jdata$date)){
      tw<-window(jzoo, start = index(jzoo)[t]-t.before, end = index(jzoo)[t]+t.after)
      tm<-time.matrix(index(tw))
      tm[tm==0]<-NA
      
      tm.w<-abs(1/(tm^expo)) #weigthted matrix
      tm.r<-tm.w*coredata(tw) #resid
      sel<-which(index(tw)==index(jzoo)[t]) #problem here for two surveys
      act<-mean(tm.r[,sel],na.rm=T) #check that sum is by col
      
      jdata$act[t]<-act
    }
    df[df$spi==un[j],"act"]<-jdata$act
  }
  
  return(df$act)
}
#----------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------
# reff.fold     Create folds for random effect structures
#--------------------------------------------------------------------------------------------
# Arguments:
# data        variable to account for random effect
# n.folds     number of folds
#
# Example:
# See 7_BRT.R

reff.fold<-function (data, n.folds){
  # function to generate a fold assignation based on a random effect structure
  # data=variable to account for random effect
  # n.folds=number of folds
  
  un<-unique(data) #unique group values
  n.cases=length(un) #number of groups
  run<-un[order(runif(n.cases,1,100))] #randomize order of groups
  fold<-rep(seq(1, n.folds, by = 1), length = n.cases) #assign folder
  
  #recode function (http://susanejohnston.wordpress.com/2012/10/01/find-and-replace-in-r-part-2-how-to-recode-many-values-simultaneously/)
  vec<-data
  for (i in run) vec[data == i] <- fold[run == i]
  vec
}
#--------------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------
# resid.autocor   Calculate Moran's I for a set of surveys
#--------------------------------------------------------------------------------------------
resid.autocor<-function(obs,pred,x,y,date,family){
  require(spdep)
  
  #create dataframe to check that dimensions are equal
  df<-data.frame(obs,pred,x,y,date)
  
  #calculate residuals (code extracted from gbm.step function in dismo package)
  y_i <- df$obs
  u_i <- df$pred
  if (family == "bernoulli") {
    deviance.contribs <- (y_i * log(u_i)) + ((1 - y_i) * log(1 - u_i))
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
  }
  if (family == "poisson") {
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - (y_i - u_i)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
  }
  if (family == "gaussian" | family == "laplace") {
    residuals <- y_i - u_i
  }
  df$resid<-residuals
  
  #set unique dates
  if(class(date)!="Date") stop("set date as Date class")
  un<-unique(date)
  resid.table<-data.frame(date=un)
  
  #loop function at the date level
  for (i in 1:length(un)){
    dfsel<-df[df$date==un[i],] #select data at date i
    resid.table$Mean.Resid[i]<-mean(dfsel$resid) #mean residuals at date i
    
    #global Moran's I (see Dormann et al 2007)
    nb<-dnearneigh(as.matrix(dfsel[3:4]),0,10000)
    listw<-nb2listw(nb)
    globMT<-moran.test(dfsel$resid,listw=listw)
    resid.table$MoranI[i]<-globMT$estimate[1]
    resid.table$MoranI.p[i]<-globMT$p
  }
  return(resid.table)
}
#--------------------------------------------------------------------------------------------
