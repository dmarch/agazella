# plot environmental variables


library(RColorBrewer)
library(rasterVis)
library(grid)
library(pals) # https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html


#---------------------------------------------------------------------
# plot_habitat        Plot environmental map
#---------------------------------------------------------------------
plot_habitat <- function(r, var_name, colr, units, cur = NULL, e = extent(-6, 17, 33, 45)){
  # cur is a RasterBrick with U and V fields
  
  # crop raster
  if (!is.null(e)) r <- crop(r, e)
  
  # if current field provided
  if (!is.null(cur)) {
    
    # crop raster
    if (!is.null(e)) cur <- crop(cur, e)
    
    # make vector plot
    p <- vectorplot(cur * 10,
                    isField = "dXY",
                    region = r,
                    margin = FALSE,
                    narrows = 4000,
                    col.regions = colr,
                    at = seq(minValue(r), maxValue(r), len = 101),
                    scales = list(draw = TRUE, xlab=list(cex=1.5)))
  }  else {
    
    # make plot without currents
    p <- levelplot(r,
                   pretty = TRUE,
                   margin = FALSE,
                   col.regions = colr,
                   at = seq(minValue(r), maxValue(r), len = 101),
                   scales = list(draw = TRUE, xlab=list(cex=1.5)))
  }

  
  # Add landmask to plot
  p <- p + latticeExtra::layer(sp.polygons(land, col="gray30", fill="grey80", lwd=1))
  
  # export plot with units
  pngfile <- paste0("img/", var_name, ".png")
  png(pngfile, width=2500, height=1800, res=300)
  print(p)
  grid.text(var_name, x=0.18, y=0.17, just="left", gp = gpar(fontsize = 16, fontface = "bold"))
  trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
  grid.text(units, 0.4, -0.02, hjust=0.8, vjust=1)
  dev.off()
  
}
#---------------------------------------------------------------------

e <- extent(-90, -20, -77, -50)

## Import landmask
data(countriesHigh, package = "rworldxtra", envir = environment())
land <- crop(countriesHigh, e)



## Bathymetry
r <- raster("D:/Data/agazella/gebco/derived_bathy.nc")
plot_habitat(r, var_name = "BAT", colr = colorRampPalette(rev(brewer.pal(9, 'Blues'))), units = "(m)", e = e)

## Slope
r <- raster("D:/Data/agazella/gebco/derived_slope.nc")
plot_habitat(r, var_name = "SLP", colr = rev(terrain.colors(100)), units = "(º)", e = e)

## Shore distance
r <- raster("D:/Data/agazella/gebco/derived_sdist.nc")
plot_habitat(r, var_name = "SDIST", colr = colorRampPalette(brewer.pal(9, 'Reds')), units = "(km)", e = e)

# Sea surface temperature
r <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS/global-analysis-forecast-phy-001-024/2019/06/20190601_global-analysis-forecast-phy-001-024_thetao.nc")
r <- setMinMax(r)
plot_habitat(r, var_name = "SST", colr = colorRampPalette(rev(brewer.pal(9, 'Spectral'))), units = "(ºC)", e = e)

# Sea surface temperature gradient
r <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS/derived-global-analysis-forecast-phy-001-024/2019/06/20190601_derived-global-analysis-forecast-phy-001-024_temgrad.nc")
r <- setMinMax(r)
plot_habitat(r, var_name = "SSTg", colr = parula(100), units = "(º)", e = e)

# Salinity
r <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS/global-analysis-forecast-phy-001-024/2019/06/20190601_global-analysis-forecast-phy-001-024_so.nc")
r <- setMinMax(r)
plot_habitat(r, var_name = "SAL", colr = colorRampPalette(rev(brewer.pal(9, 'Spectral'))), units = "(PSU)", e = e)

# Salinity gradient
r <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS/derived-global-analysis-forecast-phy-001-024/2019/06/20190601_derived-global-analysis-forecast-phy-001-024_salgrad.nc")
r <- setMinMax(r)
plot_habitat(r, var_name = "SALg", colr = parula(100), units = "(º)", e = e)

# Sea Level anomaly (With currents)
r <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS/global-analysis-forecast-phy-001-024/2019/06/20190601_global-analysis-forecast-phy-001-024_zos.nc")
r <- setMinMax(r)
v <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS/global-analysis-forecast-phy-001-024/2019/06/20190601_global-analysis-forecast-phy-001-024_vo.nc")
u <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS/global-analysis-forecast-phy-001-024/2019/06/20190601_global-analysis-forecast-phy-001-024_uo.nc")
cur <- brick(u, v)
plot_habitat(r, var_name = "SSH", cur = cur, colr = kovesi.rainbow_bgyr_35_85_c72(100), units = "(m)", e = e)

# EKE
r <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS/derived-global-analysis-forecast-phy-001-024/2019/06/20190601_derived-global-analysis-forecast-phy-001-024_eke.nc")
r <- setMinMax(r)
plot_habitat(r, var_name = "EKE", colr = rev(brewer.rdylbu(100)), units = expression((m^2 / s^2)), e = e)

# CHL
r <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_BIO_001_028-TDS/global-analysis-forecast-bio-001-028-daily/2019/06/20190601_global-analysis-forecast-bio-001-028-daily_chl.nc")
r <- setMinMax(r)
#rlog <- log(r)
plot_habitat(r, var_name = "CHL", colr = kovesi.rainbow_bgyr_35_85_c72(100), units = expression((mg / m^3)), e = e)


# Sea ice thickness
r <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS/global-analysis-forecast-phy-001-024/2019/06/20190601_global-analysis-forecast-phy-001-024_sithick.nc")
r <- setMinMax(r)
plot_habitat(r, var_name = "SIT", colr = colorRampPalette(rev(brewer.pal(9, 'Spectral'))), units = "(m)", e = e)

# Sea ice area fraction
r <- raster("D:/Data/agazella/cmems/GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS/global-analysis-forecast-phy-001-024/2019/06/20190601_global-analysis-forecast-phy-001-024_siconc.nc")
r <- setMinMax(r)
plot_habitat(r, var_name = "SIC", colr = colorRampPalette(rev(brewer.pal(9, 'Spectral'))), units = "(m)", e = e)

