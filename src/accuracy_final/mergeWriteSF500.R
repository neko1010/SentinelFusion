##Adapted from https://blogs.fu-berlin.de/reseda/area-adjusted-accuracies/

# area adjusted accuracy assessment 
# calculated as in Olofsson et al. 2014

library(raster)
library(xtable)
library(sf)

# import classification image and train and validation shapefiles
setwd("~/BSU/diss/SF_MS/data/")


## July 2021 - RFtest 500 trees
## List full paths
imgs07 = list.files('RFtest/500/', pattern = '*.tif', full.names = TRUE)
## create raster objects
ras07 = lapply(imgs07, raster)
## merge them
sf07 = do.call(merge, ras07) 

writeRaster(sf07, filename = 'RFtest/julySF500.tif', format = 'GTiff')

print("July written")

##Adapted from https://blogs.fu-berlin.de/reseda/area-adjusted-accuracies/

# area adjusted accuracy assessment 
# calculated as in Olofsson et al. 2014

library(raster)
library(xtable)
library(sf)
library(dplyr)

# import classification image and train and validation shapefiles
setwd("~/BSU/diss/SF_MS/data/")

## July 2021
img.raw = raster('RFtest/julySF500.tif')
valid <- st_read('SFval2021/val202107.shp')

## Aggregate Upland, Shadow, and Snow classes due to low counts of shadow and snow
#validation
valid$LC = case_when(valid$class == 1 | valid$class == 4 | valid$class == 5 ~ 1,
                     valid$class == 2 ~ 2,
                     valid$class == 3 ~ 3)

#classified map
from = c(1,2,3,4,5)
to = c(1,2,3,1,1)
new = cbind(from, to)
img.class = reclassify(img.raw, new)

## number of classes and labels
nclass <- 3
classes <- c(1,2,3)

# create regular accuracy matrix 
confmat <- table(as.factor(extract(img.class, valid)), as.factor(valid$LC))

# including empty column for snow
confmat = as.matrix(confmat)
colnames(confmat) = classes
confmat

imgVal = freq(img.class, progress = 'text')

maparea <- imgVal[,2][2:4] * 10 ^ 2 / 1000000

# set confidence interval
conf <- 1.96

# total  map area
A <- sum(maparea)
# proportion of area mapped as class i
W_i <- maparea / A
# number of map points per class
n_i <- rowSums(confmat) 

# population error matrix (Eq.4)
p <- W_i * confmat / n_i
p[is.na(p)] <- 0
p[is.infinite(p)] <- 0

# area estimation
p_area <- colSums(p) * A

# area estimation confidence interval (Eq.10)
p_area_CI <- conf * A * sqrt(colSums((W_i * p - p ^ 2) / (n_i - 1))) 

# overall accuracy (Eq.1)
OA <- sum(diag(p))
# producers accuracy (Eq.2)
PA <- diag(p) / colSums(p)

## swap NaNs for zeroes
PA[is.na(PA)] <- 0

# users accuracy (Eq.3)
UA <- diag(p) / rowSums(p)

## swap NaNs for zeroes
UA[is.na(UA)] <- 0

# overall accuracy confidence interval (Eq.5)
OA_CI <- conf * sqrt(sum(W_i ^ 2 * UA * (1 - UA) / (n_i - 1)))
# user accuracy confidence interval (Eq.6)
UA_CI <- conf * sqrt(UA * (1 - UA) / (n_i - 1)) 
# producer accuracy confidence interval (Eq.7)
N_j <- sapply(1:nclass, function(x) sum(maparea / n_i * confmat[ , x]))#, na.rm = T))
tmp <- sapply(1:nclass, function(x) sum(maparea[-x] ^ 2 * confmat[-x, x] / n_i[-x] * ( 1 - confmat[-x, x] / n_i[-x]) / (n_i[-x] - 1)))#,, na.rm = T) )

PA_CI <- conf * sqrt(1 / N_j ^ 2 * (maparea ^ 2 * ( 1 - PA ) ^ 2 * UA * (1 - UA) / (n_i - 1) + PA ^ 2 * tmp))
## swap NaNs for zeroe
PA_CI[is.na(PA_CI)] <- 0

# gather results
result <- matrix(c(p_area, p_area_CI, PA * 100, PA_CI * 100, UA * 100, UA_CI * 100, c(OA * 100, rep(NA, nclass-1)), c(OA_CI * 100, rep(NA, nclass-1))), nrow = nclass)
result <- round(result, digits = 2) 
rownames(result) <- c('Other', 'Mesic', 'Water') 
colnames(result) <- c("km²", "km²±", "PA", "PA±", "UA", "UA±", "OA", "OA±")
class(result) <- "table"
result

xresult = xtable(result)

print(xresult, file = '../output/accuracy/areaAdj07SF500.txt')

## conf mat
xconfmat = xtable(confmat)

print(xconfmat, file = '../output/accuracy/confmat07SF500.txt')