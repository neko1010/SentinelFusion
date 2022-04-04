library(raster)
library(xtable)
library(sf)
library(fasterize)

setwd('~/BSU/diss/SF_MS/data/comparisons/')

## Planet maps raster to points

## Planet maps
hailey.planet.july = raster('haileyCreek/planetBinary0719.tif')
hailey.planet.aug = raster('haileyCreek/planetBinary0819.tif')
hailey.planet.sep = raster('haileyCreek/planetBinary0919.tif')

## to points
hailey.pts.july = rasterToPoints(hailey.planet.july, spatial = T)
hailey.pts.aug = rasterToPoints(hailey.planet.aug, spatial = T) 
hailey.pts.sep = rasterToPoints(hailey.planet.sep, spatial = T) 

## Compare with other products 
##Sentinel fusioan
hailey.sf.july = raster('haileyCreek/SFbinary_mesic0719.tif')
hailey.sf.aug = raster('haileyCreek/SFbinary_mesic0819.tif')
hailey.sf.sep = raster('haileyCreek/SFbinary_mesic0919.tif')

##NWI and SGI polys
hailey.nwi = st_read('haileyCreekNWI.shp')
hailey.sgi = st_read('haileyCreekSGI.shp')

##Reproject
hailey.nwi.prj = st_transform(hailey.nwi, crs(hailey.planet.july))
hailey.sgi.prj = st_transform(hailey.sgi, crs(hailey.planet.july))

## fasterize
hailey.nwi.ras = fasterize(hailey.nwi.prj, hailey.planet.july)
hailey.sgi.ras = fasterize(hailey.sgi.prj, hailey.planet.july)

## assign zero to NA
hailey.nwi.ras[is.na(hailey.nwi.ras[])] <- 0
hailey.sgi.ras[is.na(hailey.sgi.ras[])] <- 0 

## categories
cats = c('Other', 'Mesic')

## create regular accuracy matrix 
## SF

## SF comparison
##July
sf.confmat.july <- table(as.factor(extract(hailey.sf.july, hailey.pts.july)), as.factor(hailey.pts.july$planetBinary0719))
rownames(sf.confmat.july) =  cats
colnames(sf.confmat.july) =  cats
sf.confmat.july
## OA, PA, UA
sf.oa.july = sum(diag(sf.confmat.july))/sum(sf.confmat.july)
sf.pa.july = diag(sf.confmat.july)/colSums(sf.confmat.july)
sf.ua.july = diag(sf.confmat.july)/rowSums(sf.confmat.july)

## August
sf.confmat.aug <- table(as.factor(extract(hailey.sf.aug, hailey.pts.aug)), as.factor(hailey.pts.aug$planetBinary0819))
rownames(sf.confmat.aug) =  cats
colnames(sf.confmat.aug) =  cats
sf.confmat.aug
## OA, PA, UA
sf.oa.aug = sum(diag(sf.confmat.aug))/sum(sf.confmat.aug)
sf.pa.aug = diag(sf.confmat.aug)/colSums(sf.confmat.aug)
sf.ua.aug = diag(sf.confmat.aug)/rowSums(sf.confmat.aug)

## September
sf.confmat.sep <- table(as.factor(extract(hailey.sf.sep, hailey.pts.sep)), as.factor(hailey.pts.sep$planetBinary0919))
rownames(sf.confmat.sep) =  cats
colnames(sf.confmat.sep) =  cats
sf.confmat.sep
## OA, PA, UA
sf.oa.sep = sum(diag(sf.confmat.sep))/sum(sf.confmat.sep)
sf.pa.sep = diag(sf.confmat.sep)/colSums(sf.confmat.sep)
sf.ua.sep = diag(sf.confmat.sep)/rowSums(sf.confmat.sep)


# gather results
sf.result <- matrix(c(sf.pa.july * 100, sf.ua.july * 100, c(sf.oa.july * 100, NA),
                      sf.pa.aug * 100, sf.ua.aug * 100, c(sf.oa.aug * 100, NA),
                      sf.pa.sep * 100, sf.ua.sep * 100, c(sf.oa.sep * 100, NA)), nrow = 2)
sf.result <- round(sf.result, digits = 2)

rownames(sf.result) <- c('Other', 'Mesic') 
colnames(sf.result) <- c("PA July",  "UA July", "OA July","PA Aug", "UA Aug", "OA Aug","PA Sep", "UA Sep", "OA Sep")
class(sf.result) <- "table"
sf.result

## NWI comparison
## July
nwi.confmat.july <- table(as.factor(extract(hailey.nwi.ras, hailey.pts.july)), as.factor(hailey.pts.july$planetBinary0719))
rownames(nwi.confmat.july) =  cats
colnames(nwi.confmat.july) =  cats
nwi.confmat.july
## OA, PA, UA
nwi.oa.july = sum(diag(nwi.confmat.july))/sum(nwi.confmat.july)
nwi.pa.july = diag(nwi.confmat.july)/colSums(nwi.confmat.july)
nwi.ua.july = diag(nwi.confmat.july)/rowSums(nwi.confmat.july)

## Aug
nwi.confmat.aug <- table(as.factor(extract(hailey.nwi.ras, hailey.pts.aug)), as.factor(hailey.pts.aug$planetBinary0819))
rownames(nwi.confmat.aug) =  cats
colnames(nwi.confmat.aug) =  cats
nwi.confmat.aug
## OA, PA, UA
nwi.oa.aug = sum(diag(nwi.confmat.aug))/sum(nwi.confmat.aug)
nwi.pa.aug = diag(nwi.confmat.aug)/colSums(nwi.confmat.aug)
nwi.ua.aug = diag(nwi.confmat.aug)/rowSums(nwi.confmat.aug)

## Sep
nwi.confmat.sep <- table(as.factor(extract(hailey.nwi.ras, hailey.pts.sep)), as.factor(hailey.pts.sep$planetBinary0919))
rownames(nwi.confmat.sep) =  cats
colnames(nwi.confmat.sep) =  cats
nwi.confmat.sep
## OA, PA, UA
nwi.oa.sep = sum(diag(nwi.confmat.sep))/sum(nwi.confmat.sep)
nwi.pa.sep = diag(nwi.confmat.sep)/colSums(nwi.confmat.sep)
nwi.ua.sep = diag(nwi.confmat.sep)/rowSums(nwi.confmat.sep)

# gather results
nwi.result <- matrix(c(nwi.pa.july * 100, nwi.ua.july * 100, c(nwi.oa.july * 100, NA),
                      nwi.pa.aug * 100, nwi.ua.aug * 100, c(nwi.oa.aug * 100, NA),
                      nwi.pa.sep * 100, nwi.ua.sep * 100, c(nwi.oa.sep * 100, NA)), nrow = 2)
nwi.result <- round(nwi.result, digits = 2)

rownames(nwi.result) <- c('Other', 'Mesic') 
colnames(nwi.result) <- c("PA July",  "UA July", "OA July","PA Aug", "UA Aug", "OA Aug","PA Sep", "UA Sep", "OA Sep")
class(nwi.result) <- "table"
nwi.result



## sgi comparison
## July
sgi.confmat.july <- table(as.factor(extract(hailey.sgi.ras, hailey.pts.july)), as.factor(hailey.pts.july$planetBinary0719))
rownames(sgi.confmat.july) =  cats
colnames(sgi.confmat.july) =  cats
sgi.confmat.july
## OA, PA, UA
sgi.oa.july = sum(diag(sgi.confmat.july))/sum(sgi.confmat.july)
sgi.pa.july = diag(sgi.confmat.july)/colSums(sgi.confmat.july)
sgi.ua.july = diag(sgi.confmat.july)/rowSums(sgi.confmat.july)

## Aug
sgi.confmat.aug <- table(as.factor(extract(hailey.sgi.ras, hailey.pts.aug)), as.factor(hailey.pts.aug$planetBinary0819))
rownames(sgi.confmat.aug) =  cats
colnames(sgi.confmat.aug) =  cats
sgi.confmat.aug
## OA, PA, UA
sgi.oa.aug = sum(diag(sgi.confmat.aug))/sum(sgi.confmat.aug)
sgi.pa.aug = diag(sgi.confmat.aug)/colSums(sgi.confmat.aug)
sgi.ua.aug = diag(sgi.confmat.aug)/rowSums(sgi.confmat.aug)

## Sep
sgi.confmat.sep <- table(as.factor(extract(hailey.sgi.ras, hailey.pts.sep)), as.factor(hailey.pts.sep$planetBinary0919))
rownames(sgi.confmat.sep) =  cats
colnames(sgi.confmat.sep) =  cats
sgi.confmat.sep
## OA, PA, UA
sgi.oa.sep = sum(diag(sgi.confmat.sep))/sum(sgi.confmat.sep)
sgi.pa.sep = diag(sgi.confmat.sep)/colSums(sgi.confmat.sep)
sgi.ua.sep = diag(sgi.confmat.sep)/rowSums(sgi.confmat.sep)

# gather results
sgi.result <- matrix(c(sgi.pa.july * 100, sgi.ua.july * 100, c(sgi.oa.july * 100, NA),
                       sgi.pa.aug * 100, sgi.ua.aug * 100, c(sgi.oa.aug * 100, NA),
                       sgi.pa.sep * 100, sgi.ua.sep * 100, c(sgi.oa.sep * 100, NA)), nrow = 2)
sgi.result <- round(sgi.result, digits = 2)

rownames(sgi.result) <- c('Other', 'Mesic') 
colnames(sgi.result) <- c("PA July",  "UA July", "OA July","PA Aug", "UA Aug", "OA Aug","PA Sep", "UA Sep", "OA Sep")
class(sgi.result) <- "table"
sgi.result

## Export latex tables #################

##SF
sf.confmat.july.x = xtable(sf.confmat.july)
print(sf.confmat.july.x, file = '../../output/archetypes/haileyCreekSFconfmatjuly.txt')
sf.result.x = xtable(sf.result)
print(sf.result.x, file = '../../output/archetypes/haileyCreekSFresult.txt')

##NWI
nwi.confmat.july.x = xtable(nwi.confmat.july)
print(nwi.confmat.july.x, file = '../../output/archetypes/haileyCreekNWIconfmatjuly.txt')
nwi.result.x = xtable(nwi.result)
print(nwi.result.x, file = '../../output/archetypes/haileyCreekNWIresult.txt')

sgi.confmat.july.x = xtable(sgi.confmat.july)
print(sgi.confmat.july.x, file = '../../output/archetypes/haileyCreekSGIconfmatjuly.txt')
sgi.result.x = xtable(sgi.result)
print(sgi.result.x, file = '../../output/archetypes/haileyCreekSGIresult.txt')

######################## BAR PLOT ########################################################
library(ggplot2)
library(RColorBrewer)

## Gather the data
mesic.product = c(rep("SF", 9), rep("NWI", 9), rep("SGI", 9))

month = rep(c(rep("July", 3), rep("August",3), rep("September",3)),3)

mesic.metric =  rep(c("PA", "UA", "OA"), 9)

## results from the tables
mesic.acc = c(sf.pa.july[2], sf.ua.july[2], sf.oa.july, sf.pa.aug[2], sf.ua.aug[2], sf.oa.aug, sf.pa.sep[2], sf.ua.sep[2], sf.oa.sep,
              nwi.pa.july[2], nwi.ua.july[2], nwi.oa.july, nwi.pa.aug[2], nwi.ua.aug[2], nwi.oa.aug, nwi.pa.sep[2], nwi.ua.sep[2], nwi.oa.sep,
              sgi.pa.july[2], sgi.ua.july[2], sgi.oa.july, sgi.pa.aug[2], sgi.ua.aug[2], sgi.oa.aug, sgi.pa.sep[2], sgi.ua.sep[2], sgi.oa.sep)

mesic.acc = mesic.acc*100

mesic = data.frame(mesic.product, month, mesic.metric, mesic.acc)


## factor the months to maintain the correct order (July, August, September)
mesic$month <- factor(mesic$month, levels = unique(mesic$month))
mesic$mesic.product <- factor(mesic$mesic.product, levels = unique(mesic$mesic.product))

##Grouped
ggplot(mesic, aes(fill = mesic.product, y = mesic.acc, x = mesic.metric)) +
  ##barplot
  geom_bar(position = 'dodge', stat = 'identity')+
  ## multipanel
  facet_wrap(~month, ncol = 3)+
  labs(fill = 'Product', y = 'Accuracy(%)', x = 'Metric') +
  scale_fill_viridis_d(direction = -1)
