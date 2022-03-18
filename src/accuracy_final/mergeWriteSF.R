##Adapted from https://blogs.fu-berlin.de/reseda/area-adjusted-accuracies/

# area adjusted accuracy assessment 
# calculated as in Olofsson et al. 2014

library(raster)
library(xtable)
library(sf)

# import classification image and train and validation shapefiles
setwd("~/BSU/diss/SF_MS/data/")

## June 2021
## List full paths
imgs06 = list.files('SF2021/june/', pattern = '*.tif', full.names = TRUE)
## create raster objects
ras06 = lapply(imgs06, raster)
## merge them
sf06 = do.call(merge, ras06) 

writeRaster(sf06, filename = 'SF2021/june/juneSF.tif', format = 'GTiff')
print("June written")


## July 2021
## List full paths
imgs07 = list.files('SF2021/july/', pattern = '*.tif', full.names = TRUE)
## create raster objects
ras07 = lapply(imgs07, raster)
## merge them
sf07 = do.call(merge, ras07) 

writeRaster(sf07, filename = 'SF2021/july/julySF.tif', format = 'GTiff')

print("July written")

## Aug 2021
## List full paths
imgs08 = list.files('SF2021/aug/', pattern = '*.tif', full.names = TRUE)
## create raster objectsi
ras08 = lapply(imgs08, raster)
## merge them
sf08 = do.call(merge, ras08) 

writeRaster(sf08, filename = 'SF2021/aug/augSF.tif', format = 'GTiff')

print("August written")

## Sep 2021
## List full paths
imgs09 = list.files('SF2021/sep/', pattern = '*.tif', full.names = TRUE)
## create raster objects
ras09 = lapply(imgs09, raster)
## merge them
sf09 = do.call(merge, ras09) 

writeRaster(sf09, filename = 'SF2021/sep/sepSF.tif')

print("September written")