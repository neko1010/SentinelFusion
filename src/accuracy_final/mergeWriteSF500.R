##Adapted from https://blogs.fu-berlin.de/reseda/area-adjusted-accuracies/

# area adjusted accuracy assessment 
# calculated as in Olofsson et al. 2014

library(raster)
library(xtable)
library(sf)

# import classification image and train and validation shapefiles
setwd("~/BSU/diss/SF_MS/data/")


## July 2021 - RFtest 200 trees
## List full paths
imgs07 = list.files('RFtest/500/', pattern = '*.tif', full.names = TRUE)
## create raster objects
ras07 = lapply(imgs07, raster)
## merge them
sf07 = do.call(merge, ras07) 

writeRaster(sf07, filename = 'RFtest/julySF500.tif', format = 'GTiff')

print("July written")

