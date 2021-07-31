#---------------------------------------------------------------------------------------------------
# plot_brt_boot_partial          Plot BRT partial effects using bootstrap
#---------------------------------------------------------------------------------------------------

mod_code <- "brt"
cores <- 50
bootstrap <- T
n_boot <- 50
sp_code <- "GAZ"  # species code

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste(output_data, "habitat-model-v2", sp_code, mod_code, sep="/")
outdir <- paste(output_data, "habitat-model-v2", sp_code, mod_code, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

outdir <- paste(output_data, "habitat-model-v2", sp_code, mod_code, sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


# import model
mod <- readRDS(paste0(indir, "/", sp_code, "_", mod_code, ".rds"))


# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/")
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)
n_models <- length(models)

# make a list of values to predict per variable from a single model
n_res <- 100
gbm_list <- ggBRT::plot.gbm.4list(models[[1]], continuous.resolution = n_res)

# get predictor names
pred.names <- models[[1]]$var.names
n_var <- length(pred.names)

# create empty matrix to store data
boot_mat <- array(NA, dim=c(n_res, n_var, n_boot))

for(i in 1:length(models)){
  # get model
  mi <- models[[i]]
  
  # predict values for list of values
  ipred_boot <- ggBRT::plot.gbm.boot(mi, list.4.preds = gbm_list, continuous.resolution = n_res)
  
  # append data
  #boot_predicts[[i]] <- ipred_boot
  boot_mat[,,i] <- ipred_boot
}


# calculate median and CI per variable
boot_med <- apply(boot_mat, c(1,2), median, na.rm=T)
boot_cil <- apply(boot_mat, c(1,2), quantile, prob = 0.025, na.rm=T)
boot_ciu <- apply(boot_mat, c(1,2), quantile, prob = 0.975, na.rm=T)


# create a data.frame
data_list <- list()
for(i in 1:n_var){
  
  # create data.frame
  idf <- data.frame(
    var = pred.names[i],
    xval = gbm_list[[i]]$X1,
    med = boot_med[,i],
    cil = boot_cil[,i],
    ciu = boot_ciu[,i]
    )

  #append
  data_list[[i]] <- idf
}

# combine data
data <- rbindlist(data_list)


# relative importance


mod$contributions$var

data$var <- factor(data$var, levels = mod$contributions$var)
relinf <- round(mod$contributions$rel.inf, 1)
labels <- paste0(mod$contributions$var, " (", relinf, "%)")
names(labels) <- mod$contributions$var

# select number of variables to plot
n_plots <- 6
data2 <- filter(data, var %in% mod$contributions$var[1:n_plots])


# plot
p <- ggplot(data2, aes(x = xval)) +
  geom_ribbon(aes(ymin = cil, ymax = ciu), fill="steelblue", alpha=.2, linetype=0) +
  geom_line(aes(y = med), color="steelblue") +
  ylab("Fitted function") + xlab("") +
  facet_wrap(var~., scales = "free_x", ncol =2, strip.position = "bottom", labeller=labeller(var=labels)) +
  theme_article(base_size = 14) +
  theme(
    strip.placement = "outside",
    plot.margin = unit(c(10,10,10,10), "points"),
    axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0))
  )


# export plot
outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_response_boot.png")
ggsave(outfile, p, width=18, height=22, units="cm", dpi=300)



