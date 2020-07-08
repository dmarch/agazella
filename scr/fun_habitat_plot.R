#-----------------------------------------------------------------------------------------
# fun_habitat_plot      Suite of common functions for plotting habitat model data
#-----------------------------------------------------------------------------------------
# density.plot     Density plots of environmental data on observed and simulated locations
# plot_Missing    Plot missing data
# radarPlot       Radar plot of variable contribution from models





#------------------------------------------------------------------------------------
# density.plot     Density plots of environmental data on observed and simulated locations
#------------------------------------------------------------------------------------
density.plot <- function(title="", xlab="SST (ºC)", legend="", alpha=0.35, data=data, var=SST, group=type, cols = c("#d7191c", "#2c7bb6")){
  
  g <- ggplot(data, aes(x=var, color=group)) +
    geom_line(stat="density", size = 1, alpha = 1.0) +
    scale_color_manual(values=cols) +
    labs(title = title, x = xlab, fill="") +
    theme_light() 
  return(g)
  
}
#------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# plot_Missing    Plot missing data
#------------------------------------------------------------------------------------
plot_Missing <- function(data_in, title = NULL){
  # https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("grey80", "grey10"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}
#------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------
# radarPlot       Radar plot of variable contribution from models
#-----------------------------------------------------------------------------------
radarPlot <- function(var_imp, var_order, colors_border=rgb(0.2,0.5,0.5,0.9), colors_in=rgb(0.2,0.5,0.5,0.4)){
  
  # set parameters for plot
  min_val <- 0
  max_val <- ceiling(max(var_imp))#100
  nseg <- 4
  
  # prepare data.frame
  var_imp <- dplyr::select(data.frame(t(var_imp)), var_order)
  data <- rbind(rep(max_val, length(var_imp)) , rep(min_val, length(var_imp)), var_imp)
  data <- data.frame(data)
  row.names(data) <- c("max", "min", "MaxEnt")
  
  radarchart(data  , axistype=1 , 
             #custom polygon
             pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
             # number of segments
             seg=nseg,
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=round(seq(min_val,max_val,max_val/nseg),1), cglwd=0.8,
             #custom labels
             vlcex=1)
}
#-----------------------------------------------------------------------------------
