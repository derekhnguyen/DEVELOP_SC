#############Load UVVR Rasters##################
library("rgdal")
Lan_Ban <- raster("UVVR_Landsat_Bands.tif", band=7)
Lan_Ban@extent

# calculate and save the min and max values of the raster to the raster object
Lan_Ban <- setMinMax(Lan_Ban)

# view raster attributes, copied what appears in the console
Lan_Ban
# class      : RasterLayer
# band       : 1  (of  7  bands)
# dimensions : 1351, 2086, 2818186  (nrow, ncol, ncell)
# resolution : 0.0002694946, 0.0002694946  (x, y)
# extent     : -79.85044, -79.28827, 32.75653, 33.12062  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
# source     : /Users/adrianalecompte/Desktop/R/UVVR-analysis/UVVR_Landsat_Bands.tif
# names      : UVVR_Landsat_Bands
# values     : 0, 20000  (min, max)

#plot raster, add title with main
plot(Lan_Ban, main= "UVVR Landsat Bands")

?plot

#distribution of values in the raster
hist(Lan_Ban, main="Distribution of Landsat Bands in UVVR", 
     col= "purple", 
     maxpixels=22000000)

?gbm.interactions

############ BRT code from MSU Spatial & Community Ecology lab #####################
library(dismo)     # package to run the model
library(gbm)       # GBM contains boosted regression tree functions 
#load("~/Downloads/modeldata.RData") = required by example, switched in code modeldata for Lan_Ban

files <- list.files(path=paste(system.file(package="dismo"), "/ex",  sep= ""), pattern= "grd", full.names=TRUE )
predictors <- stack(files)

#Build initial model
brt_model <- gbm.step(data=Lan_Ban, gbm.x = 9, gbm.y = 1)

?gbm.step
#Make predictions across raster layer
predictions <- predict(predictors, brt_model, n.trees=brt_model$gbm.call$best.trees, type="response") 

#Make plot
plot(predictions, main="BRT prediction - full")

#Build initial model
brt_model <- gbm.step(data=Lan_Ban, gbm.x = c(2, 3, 6), gbm.y = 1)

#Make predictions across raster layer
predictions <- predict(predictors, brt_model, n.trees=brt_model$gbm.call$best.trees, type="response")

#Make plot
plot(predictions, main="BRT prediction - reduced")