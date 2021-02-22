


library("rnaturalearthdata")
library("rnaturalearth")
world <- ne_countries(scale = "medium", returnclass = "sp")
land <- crop(world, extent(-90, -20, -80, -40))


input_data <- "data/out/habitat_model/observations/"


data <- read.csv(obs_file)
coordinates(data) <- ~ lon + lat
proj4string(data) <- "+proj=longlat +ellps=WGS84"
cp <- mcp(data, percent = 100) # MCP use all the positions

# Extend the MCP with a buffer of 1 degree
cp.buf <- gBuffer(cp, width = 1)





output_data <- paste("data/out/habitat_model/predict", predict_folder, sp_code, mod_code, sep="/")

date <- as.Date("2019-04-01")
YYYY <- year(date)
MM <- sprintf("%02d", month(date))
product_folder <- paste(output_data, YYYY, MM, sep="/")  # Set folder
outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred.nc")

pred <- raster(outfile)

plot(pred, main = paste(sp_name, "   Model:", mod_code), zlim=c(0,1), col = viridis(100))
plot(land, col="grey80", border="grey60", add=TRUE)
text(x = -3.5, y = 44, labels = date)
box()


#
pred <- crop(pred, cp.buf)
pred <- mask(pred, cp.buf)

