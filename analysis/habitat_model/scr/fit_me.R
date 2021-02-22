#---------------------------------------------------------------------------------------------------
# fit_sdm             Fit Species distribution model
#---------------------------------------------------------------------------------------------------




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
# MaxEnt model
#-----------------------------------------------------------------

mod_code <- "me"

set.seed(131)

# Separate response variable and covariates
response <- dplyr::select(data, occ)
covar <- dplyr::select(data, vars)

# Fit model
mod <- maxent(covar, response)

# Save model
#save(mod, file = paste0(output_data, "/", sp_code, "_", mod_code, ".Rdata"))  # save model
saveRDS(mod, file = paste0(outdir, "/", sp_code, "_", mod_code, ".rds"))  # save model


# Plot variable contribution
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_var_contrib.png")
png(pngfile, width=1500, height=1000, res=200)
plot(mod)
dev.off()

# Plot variable contribution using radar plot
var_imp <- plot(mod)
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_var_radar.png")
png(pngfile, width=1000, height=1000, res=150)
radarPlot(var_imp, var_order=vars)
dev.off()

# Plot response curves
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_response.png")
png(pngfile, width=1500, height=1000, res=200)
response(mod)
dev.off()

