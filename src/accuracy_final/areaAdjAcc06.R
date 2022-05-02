##Adapted from https://blogs.fu-berlin.de/reseda/area-adjusted-accuracies/

# area adjusted accuracy assessment 
# calculated as in Olofsson et al. 2014

library(raster)
library(xtable)
library(sf)
library(dplyr)

# import classification image and train and validation shapefiles
setwd("~/BSU/diss/SF_MS/data/")

## June 2021
img.class = raster('SF2021/june/juneSF.tif')
img.class.nosar = raster('SF2021/juneNoSAR/juneSFNoSAR.tif')
valid <- st_read('SFval2021/val202106.shp')


## number of classes and labels
nclass <- 5
classes <- c(1,2,3,4,5)

# create regular accuracy matrix 
confmat <- table(as.factor(extract(img.class, valid)), as.factor(valid$class))
confmat.nosar <- table(as.factor(extract(img.class.nosar, valid)), as.factor(valid$class))

# confusion matrices
confmat = as.matrix(confmat)
confmat.nosar = as.matrix(confmat.nosar)
colnames(confmat) = classes
colnames(confmat.nosar) = classes
confmat
confmat.nosar

imgVal = freq(img.class, progress = 'text')

maparea <- imgVal[,2][2:6] * 10 ^ 2 / 1000000

# set confidence interval
conf <- 1.96

# total  map area
A <- sum(maparea)
# proportion of area mapped as class i
W_i <- maparea / A ## should be strata area of original map (SF map)
# number of map points per class
n_i <- rowSums(confmat)
n_i.nosar <- rowSums(confmat.nosar)

# population error matrix (Eq.4)
p <- W_i * confmat / n_i
#p[is.na(p)] <- 0
#p[is.infinite(p)] <- 0

p.nosar <- W_i * confmat.nosar / n_i.nosar
#p.nosar[is.na(p.nosar)] <- 0
#p.nosar[is.infinite(p.nosar)] <- 0

# area estimation
p_area <- colSums(p) * A 

# area estimation confidence interval (Eq.10)
p_area_CI <- conf * A * sqrt(colSums((W_i * p - p ^ 2) / (n_i - 1))) 

# overall accuracy (Eq.1)
OA <- sum(diag(p))
OA.nosar <- sum(diag(p.nosar))

# producers accuracy (Eq.2)
PA <- diag(p) / colSums(p)
PA.nosar <- diag(p.nosar) / colSums(p.nosar)

## swap NaNs for zeroes
#PA[is.na(PA)] <- 0
#PA.nosar[is.na(PA.nosar)] <- 0

# users accuracy (Eq.3)
UA <- diag(p) / rowSums(p)
UA.nosar <- diag(p.nosar) / rowSums(p.nosar)

## swap NaNs for zeroes
#UA[is.na(UA)] <- 0
#UA.nosar[is.na(UA.nosar)] <- 0

# overall accuracy confidence interval (Eq.5)
OA_CI <- conf * sqrt(sum(W_i ^ 2 * UA * (1 - UA) / (n_i - 1)))
OA_CI.nosar <- conf * sqrt(sum(W_i ^ 2 * UA.nosar * (1 - UA.nosar) / (n_i.nosar - 1), na.rm = T))
# user accuracy confidence interval (Eq.6)
UA_CI <- conf * sqrt(UA * (1 - UA) / (n_i - 1)) 
UA_CI.nosar <- conf * sqrt(UA.nosar * (1 - UA.nosar) / (n_i.nosar - 1)) 
# producer accuracy confidence interval (Eq.7)
N_j <- sapply(1:nclass, function(x) sum(maparea / n_i * confmat[ , x]))#, na.rm = T))
N_j.nosar <- sapply(1:nclass, function(x) sum(maparea / n_i.nosar * confmat.nosar[ , x]))#, na.rm = T))
tmp <- sapply(1:nclass, function(x) sum(maparea[-x] ^ 2 * confmat[-x, x] / n_i[-x] * ( 1 - confmat[-x, x] / n_i[-x]) / (n_i[-x] - 1), na.rm = T) )
tmp.nosar <- sapply(1:nclass, function(x) sum(maparea[-x] ^ 2 * confmat.nosar[-x, x] / n_i.nosar[-x] * ( 1 - confmat.nosar[-x, x] / n_i.nosar[-x]) / (n_i.nosar[-x] - 1), na.rm = T) )

PA_CI <- conf * sqrt(1 / N_j ^ 2 * (maparea ^ 2 * ( 1 - PA ) ^ 2 * UA * (1 - UA) / (n_i - 1) + PA ^ 2 * tmp))
PA_CI.nosar <- conf * sqrt(1 / N_j.nosar ^ 2 * (maparea ^ 2 * ( 1 - PA.nosar ) ^ 2 * UA.nosar * (1 - UA.nosar) / (n_i.nosar - 1) + PA.nosar ^ 2 * tmp.nosar))

## swap NaNs for zeroe
#PA_CI[is.na(PA_CI)] <- 0
#PA_CI.nosar[is.na(PA_CI.nosar)] <- 0

# gather results
areas = round(c(p_area, p_area_CI), 0)
acc_metrics =  round(c(PA * 100, PA_CI * 100, UA * 100, UA_CI * 100, c(OA * 100, rep(NA, nclass-1)), c(OA_CI * 100, rep(NA, nclass-1))), 2)
acc_metrics.nosar =  round(c(PA.nosar * 100, PA_CI.nosar * 100, UA.nosar * 100, UA_CI.nosar * 100, c(OA.nosar * 100, rep(NA, nclass-1)), c(OA_CI.nosar * 100, rep(NA, nclass-1))), 2)
result = matrix(c(areas, acc_metrics, acc_metrics.nosar), nrow = nclass)

#result <- matrix(c(p_area, p_area_CI, PA * 100, PA_CI * 100, UA * 100, UA_CI * 100, c(OA * 100, rep(NA, nclass-1)), c(OA_CI * 100, rep(NA, nclass-1))), nrow = nclass)
#result <- round(result, digits = 2) 
rownames(result) <- c('Upland', 'Mesic', 'Water', 'Snow', 'Shadow') 
colnames(result) <- c("km²", "km²±", "PA", "PA±", "UA", "UA±", "OA", "OA±", "PA", "PA±", "UA", "UA±", "OA", "OA±")
#class(result) <- "table"
result

xresult = xtable(result)

#print(xresult, file = '../output/accuracy/areaAdj06.txt')
print(xresult, type = 'html', file = '../output/accuracy/areaAdj06.html')

## conf mat
rownames(confmat) = c('Upland', 'Mesic', 'Water', 'Snow', 'Shadow') 
colnames(confmat) = c('Upland', 'Mesic', 'Water', 'Snow', 'Shadow') 
xconfmat = xtable(confmat)

rownames(confmat.nosar) = c('Upland', 'Mesic', 'Water', 'Snow', 'Shadow') 
colnames(confmat.nosar) = c('Upland', 'Mesic', 'Water', 'Snow', 'Shadow') 
xconfmat.nosar = xtable(confmat.nosar)


#print(xconfmat, file = '../output/accuracy/confmat06.txt')
print(xconfmat, type = 'html',  file = '../output/accuracy/confmat06.html')
print(xconfmat.nosar, type = 'html',  file = '../output/accuracy/confmat06nosar.html')