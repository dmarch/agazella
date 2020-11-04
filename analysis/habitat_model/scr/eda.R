#---------------------------------------------------------------------------------------------------
# eda.R          Exploratory data analysis
#---------------------------------------------------------------------------------------------------



library(corrplot)

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
input_data <- "data/out/habitat_model/observations/"
output_data <- paste0("results/habitat_model/", sp_code)
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#-----------------------------------------------------------------
# Import observations
#-----------------------------------------------------------------

obs_file <- paste0(input_data, sp_code, "_observations.csv")
data <- read.csv(obs_file)


#-----------------------------------------------------------------
# Explore missing data in observed data
#-----------------------------------------------------------------

## Select observed data
df <- data %>% dplyr::filter(occ == 1) %>% dplyr::select(vars)

## Plot missing data per variable
pngfile <- paste0(output_data, "/", sp_code, "_eda_missing.png")
png(pngfile, width=1500, height=1000, res=200)
p <- plot_Missing(df)
print(p)
dev.off()


#-----------------------------------------------------------------
# Explore the correlation between variables in observed data
#-----------------------------------------------------------------

# calcualate correlations using Pearson
correlations <- cor(na.omit(dplyr::select(df, vars)), method="pearson")

# plot correlations
pngfile <- paste0(output_data, "/", sp_code, "_eda_corplot.png")
png(pngfile, width=2500, height=2000, res=300)
corrplot(correlations, method="circle", tl.col = "black", order = "original", diag=FALSE, type="upper")
dev.off()


#-----------------------------------------------------------------
# Explore distribution of data per variables in both observed and simulated
#-----------------------------------------------------------------

# convert to data.frame
data <- as.data.frame(data)
data$type <- NA
data$type[data$occ == 1] <- "pres"
data$type[data$occ == 0] <- "abs"


# create plot per variable
p1 <- density.plot(title="", xlab="BAT", legend="", alpha=0.35, data=data, var=data$BAT, group=data$type)
p2 <- density.plot(title="", xlab="SLP", legend="", alpha=0.35, data=data, var=data$SLP, group=data$type)
p3 <- density.plot(title="", xlab="SDIST", legend="", alpha=0.35, data=data, var=data$SDIST, group=data$type)
p4 <- density.plot(title="", xlab="SST", legend="", alpha=0.35, data=data, var=data$SST, group=data$type)
p5 <- density.plot(title="", xlab="SSTg", legend="", alpha=0.35, data=data, var=data$SSTg, group=data$type)
p6 <- density.plot(title="", xlab="SAL", legend="", alpha=0.35, data=data, var=data$SAL, group=data$type)
p7 <- density.plot(title="", xlab="SALg", legend="", alpha=0.35, data=data, var=data$SALg, group=data$type)
p8 <- density.plot(title="", xlab="SSH", legend="", alpha=0.35, data=data, var=data$SSH, group=data$type)
p9 <- density.plot(title="", xlab="log(EKE)", legend="", alpha=0.35, data=data, var=log1p(data$EKE), group=data$type)
p10 <- density.plot(title="", xlab="log(CHL)", legend="", alpha=0.35, data=data, var=log1p(data$CHL), group=data$type)
p11 <- density.plot(title="", xlab="SIC", legend="", alpha=0.35, data=data, var=data$SIC, group=data$type)
p12 <- density.plot(title="", xlab="SIT", legend="", alpha=0.35, data=data, var=data$SIT, group=data$type)
p13 <- density.plot(title="", xlab="MLD", legend="", alpha=0.35, data=data, var=data$MLD, group=data$type)


# create layaout
lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,8),
             c(9,10),
             c(11,12),
             c(13,NA))
p <- grid.arrange(p1, p2, p3, p4,
                  p5, p6, p7, p8,
                  p9, p10, p11, p12, p13,
                  layout_matrix = lay)

# plot
pngfile <- paste0(output_data, "/", sp_code, "_eda_density.png")
png(pngfile, width=3000, height=3500, res=300)
grid.draw(p)
dev.off()




