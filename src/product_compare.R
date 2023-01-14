library(raster)
library(xtable)
library(sf)

setwd('~/BSU/diss/SF_MS/data/comparisons/')

## Planet maps raster to points

## Planet maps
mackay.planet = raster('2021/planetBinaryMackay0721.tif')
thou.planet = raster('2021/planetBinaryThousprings0721.tif')
biglost.planet = raster('2021/planetBinaryBiglost0721.tif')

## to points
mackay.pts = rasterToPoints(mackay.planet, spatial = T) 
thou.pts = rasterToPoints(thou.planet, spatial = T) 
biglost.pts = rasterToPoints(biglost.planet, spatial = T) 

## Compare with SF
mackay.sf = raster('2021/SFbinaryMackay0721.tif')
thou.sf = raster('2021/SFbinaryThousprings0721.tif')
biglost.sf = raster('2021/SFbinaryBiglost0721.tif')

## categories
cats = c('Land', 'Water')
## create regular accuracy matrix 
## SF

## Mackay
mackay.sf.confmat <- table(as.factor(extract(mackay.sf, mackay.pts)), as.factor(mackay.pts$planetBinaryMackay0721))
rownames(mackay.sf.confmat) =  cats
colnames(mackay.sf.confmat) =  cats
mackay.sf.confmat
## OA, PA, UA
mackay.sf.oa = sum(diag(mackay.sf.confmat))/sum(mackay.sf.confmat)
mackay.sf.pa = diag(mackay.sf.confmat)/colSums(mackay.sf.confmat)
mackay.sf.ua = diag(mackay.sf.confmat)/rowSums(mackay.sf.confmat)

# gather results
mackay.sf.result <- matrix(c(mackay.sf.pa * 100, mackay.sf.ua * 100, c(mackay.sf.oa * 100, NA)), nrow = 2)
mackay.sf.result <- round(mackay.sf.result, digits = 2) 
rownames(mackay.sf.result) <- c('Land', 'Water') 
colnames(mackay.sf.result) <- c("PA",  "UA", "OA")
class(mackay.sf.result) <- "table"
mackay.sf.result


## Thousand springs
thou.sf.confmat <- table(as.factor(extract(thou.sf, thou.pts)), as.factor(thou.pts$planetBinaryThousprings0721))
rownames(thou.sf.confmat) = cats
colnames(thou.sf.confmat) = cats
thou.sf.confmat
## OA, PA, UA
thou.sf.oa = sum(diag(thou.sf.confmat))/sum(thou.sf.confmat)
thou.sf.pa = diag(thou.sf.confmat)/colSums(thou.sf.confmat)
thou.sf.ua = diag(thou.sf.confmat)/rowSums(thou.sf.confmat)

# gather results
thou.sf.result <- matrix(c(thou.sf.pa * 100, thou.sf.ua * 100, c(thou.sf.oa * 100, NA)), nrow = 2)
thou.sf.result <- round(thou.sf.result, digits = 2) 
rownames(thou.sf.result) <- c('Land', 'Water') 
colnames(thou.sf.result) <- c("PA",  "UA", "OA")
class(thou.sf.result) <- "table"
thou.sf.result


## Big Lost
biglost.sf.confmat <- table(as.factor(extract(biglost.sf, biglost.pts)), as.factor(biglost.pts$planetBinaryBiglost0721))
rownames(biglost.sf.confmat) = cats
colnames(biglost.sf.confmat) = cats
biglost.sf.confmat
## OA, PA, UA
biglost.sf.oa = sum(diag(biglost.sf.confmat))/sum(biglost.sf.confmat)
biglost.sf.pa = diag(biglost.sf.confmat)/colSums(biglost.sf.confmat)
biglost.sf.ua = diag(biglost.sf.confmat)/rowSums(biglost.sf.confmat)

# gather results
biglost.sf.result <- matrix(c(biglost.sf.pa * 100, biglost.sf.ua * 100, c(biglost.sf.oa * 100, NA)), nrow = 2)
biglost.sf.result <- round(biglost.sf.result, digits = 2) 
rownames(biglost.sf.result) <- c('Land', 'Water') 
colnames(biglost.sf.result) <- c("PA",  "UA", "OA")
class(biglost.sf.result) <- "table"
biglost.sf.result

##GLAD ##########################

mackay.GLAD = raster('2021/GLADbinaryMackay0721.tif')
thou.GLAD = raster('2021/GLADbinaryThousprings0721.tif')
biglost.GLAD = raster('2021/GLADbinaryBiglost0721.tif')

## Mackay
mackay.GLAD.confmat <- table(as.factor(extract(mackay.GLAD, mackay.pts)), as.factor(mackay.pts$planetBinaryMackay0721))
rownames(mackay.GLAD.confmat) = cats
colnames(mackay.GLAD.confmat) = cats
mackay.GLAD.confmat
## OA, PA, UA
mackay.GLAD.oa = sum(diag(mackay.GLAD.confmat))/sum(mackay.GLAD.confmat)
mackay.GLAD.pa = diag(mackay.GLAD.confmat)/colSums(mackay.GLAD.confmat)
mackay.GLAD.ua = diag(mackay.GLAD.confmat)/rowSums(mackay.GLAD.confmat)

# gather results
mackay.GLAD.result <- matrix(c(mackay.GLAD.pa * 100, mackay.GLAD.ua * 100, c(mackay.GLAD.oa * 100, NA)), nrow = 2)
mackay.GLAD.result <- round(mackay.GLAD.result, digits = 2) 
rownames(mackay.GLAD.result) <- c('Land', 'Water') 
colnames(mackay.GLAD.result) <- c("PA",  "UA", "OA")
class(mackay.GLAD.result) <- "table"
mackay.GLAD.result

## Thousand springs
thou.GLAD.confmat <- table(as.factor(extract(thou.GLAD, thou.pts)), as.factor(thou.pts$planetBinaryThousprings0721))
rownames(thou.GLAD.confmat) = cats
colnames(thou.GLAD.confmat) = cats
thou.GLAD.confmat
## OA, PA, UA
thou.GLAD.oa = sum(diag(thou.GLAD.confmat))/sum(thou.GLAD.confmat)
thou.GLAD.pa = diag(thou.GLAD.confmat)/colSums(thou.GLAD.confmat)
thou.GLAD.ua = diag(thou.GLAD.confmat)/rowSums(thou.GLAD.confmat)

# gather results
thou.GLAD.result <- matrix(c(thou.GLAD.pa * 100, thou.GLAD.ua * 100, c(thou.GLAD.oa * 100, NA)), nrow = 2)
thou.GLAD.result <- round(thou.GLAD.result, digits = 2) 
rownames(thou.GLAD.result) <- c('Land', 'Water') 
colnames(thou.GLAD.result) <- c("PA",  "UA", "OA")
class(thou.GLAD.result) <- "table"
thou.GLAD.result


## Big Lost
biglost.GLAD.confmat <- table(as.factor(extract(biglost.GLAD, biglost.pts)), as.factor(biglost.pts$planetBinaryBiglost0721))
biglost.GLAD.confmat <-rbind(biglost.GLAD.confmat, c(0,0))  ## no water detected - need to append a row of zeroes
rownames(biglost.GLAD.confmat) = cats
colnames(biglost.GLAD.confmat) = cats
biglost.GLAD.confmat


## OA, PA, UA
biglost.GLAD.oa = sum(diag(biglost.GLAD.confmat))/sum(biglost.GLAD.confmat)
biglost.GLAD.pa = diag(biglost.GLAD.confmat)/colSums(biglost.GLAD.confmat)
biglost.GLAD.ua = diag(biglost.GLAD.confmat)/rowSums(biglost.GLAD.confmat)

# gather results
biglost.GLAD.result <- matrix(c(biglost.GLAD.pa * 100, biglost.GLAD.ua * 100, c(biglost.GLAD.oa * 100, NA)), nrow = 2)
biglost.GLAD.result <- round(biglost.GLAD.result, digits = 2) 
rownames(biglost.GLAD.result) <- c('Land', 'Water') 
colnames(biglost.GLAD.result) <- c("PA",  "UA", "OA")
class(biglost.GLAD.result) <- "table"
biglost.GLAD.result


################### Aggregates ######################
## Combine the tables
## SF
sf.confmat = mackay.sf.confmat + thou.sf.confmat + biglost.sf.confmat
rownames(sf.confmat) = cats
colnames(sf.confmat) = cats
sf.confmat
## OA, PA, UA
sf.oa = sum(diag(sf.confmat))/sum(sf.confmat)
sf.pa = diag(sf.confmat)/colSums(sf.confmat)
sf.ua = diag(sf.confmat)/rowSums(sf.confmat)

# gather results
sf.result <- matrix(c(sf.pa * 100, sf.ua * 100, c(sf.oa * 100, NA)), nrow = 2)
sf.result <- round(sf.result, digits = 2) 
rownames(sf.result) <- c('Land', 'Water') 
colnames(sf.result) <- c("PA",  "UA", "OA")
class(sf.result) <- "table"
sf.result

## GLAD
GLAD.confmat = mackay.GLAD.confmat + thou.GLAD.confmat + biglost.GLAD.confmat
rownames(GLAD.confmat) = cats
colnames(GLAD.confmat) = cats
GLAD.confmat
## OA, PA, UA
GLAD.oa = sum(diag(GLAD.confmat))/sum(GLAD.confmat)
GLAD.pa = diag(GLAD.confmat)/colSums(GLAD.confmat)
GLAD.ua = diag(GLAD.confmat)/rowSums(GLAD.confmat)

# gather results
GLAD.result <- matrix(c(GLAD.pa * 100, GLAD.ua * 100, c(GLAD.oa * 100, NA)), nrow = 2)
GLAD.result <- round(GLAD.result, digits = 2) 
rownames(GLAD.result) <- c('Land', 'Water') 
colnames(GLAD.result) <- c("PA",  "UA", "OA")
class(GLAD.result) <- "table"
GLAD.result

## Checking counts for sanity
sum(sf.confmat)
sum(GLAD.confmat)


## Export latex tables #################

##SF
mackay.sf.confmat.x = xtable(mackay.sf.confmat)
print(mackay.sf.confmat.x, file = '../../output/archetypes/mackaySFconfmat0721.txt')
mackay.sf.result.x = xtable(mackay.sf.result)
print(mackay.sf.result.x, file = '../../output/archetypes/mackaySFresult0721.txt')

thou.sf.confmat.x = xtable(thou.sf.confmat)
print(thou.sf.confmat.x, file = '../../output/archetypes/thouSF0721.txt')
thou.sf.result.x = xtable(thou.sf.result)
print(thou.sf.result.x, file = '../../output/archetypes/thouSFresult0721.txt')

biglost.sf.confmat.x = xtable(thou.sf.confmat)
print(biglost.sf.confmat.x, file = '../../output/archetypes/biglostSF0721.txt')
biglost.sf.result.x = xtable(biglost.sf.result)
print(biglost.sf.result.x, file = '../../output/archetypes/biglostSFresult0721.txt')

##GLAD
mackay.GLAD.confmat.x = xtable(mackay.GLAD.confmat)
print(mackay.GLAD.confmat.x, file = '../../output/archetypes/mackayGLAD0721.txt')
mackay.GLAD.result.x = xtable(mackay.GLAD.result)
print(mackay.GLAD.result.x, file = '../../output/archetypes/mackayGLADresult0721.txt')

thou.GLAD.confmat.x = xtable(thou.GLAD.confmat)
print(thou.GLAD.confmat.x, file = '../../output/archetypes/thouGLAD0721.txt')
thou.GLAD.result.x = xtable(thou.GLAD.result)
print(thou.GLAD.result.x, file = '../../output/archetypes/thouGLADresult0721.txt')

biglost.GLAD.confmat.x = xtable(biglost.GLAD.confmat)
print(biglost.GLAD.confmat.x, file = '../../output/archetypes/biglostGLAD0721.txt')
biglost.GLAD.result.x = xtable(biglost.GLAD.result)
print(biglost.GLAD.result.x, file = '../../output/archetypes/biglostGLADresult0721.txt')

#### ALL ###
sf.confmat.x = xtable(sf.confmat)
print(sf.confmat.x, file = '../../output/archetypes/SFconfmat0721.txt')
sf.result.x = xtable(sf.result)
print(sf.result.x, file = '../../output/archetypes/SFresult0721.txt')

GLAD.confmat.x = xtable(GLAD.confmat)
print(GLAD.confmat.x, file = '../../output/archetypes/GLADconfmat0721.txt')
GLAD.result.x = xtable(GLAD.result)
print(GLAD.result.x, file = '../../output/archetypes/GLADresult0721.txt')

######################## BAR PLOT ########################################################
library(ggplot2)
library(RColorBrewer)

########################### SURFACE WATER #####################################i
## Gather the data
product = rep(c(rep("SF", 3), rep("GLAD", 3)), 4)

site = c(rep("Reservoir", 6), rep("Spring-fed",6), 
         rep("Riparian",6), rep("Aggregate", 6))

metric =  rep(c("PA", "UA", "OA"), 8)

## results from the tables
acc = c(mackay.sf.pa[2], mackay.sf.ua[2], mackay.sf.oa, mackay.GLAD.pa[2], mackay.GLAD.ua[2], mackay.GLAD.oa,
        thou.sf.pa[2], thou.sf.ua[2], thou.sf.oa, thou.GLAD.pa[2], thou.GLAD.ua[2], thou.GLAD.oa,
        biglost.sf.pa[2], biglost.sf.ua[2], biglost.sf.oa, biglost.GLAD.pa[2], biglost.GLAD.ua[2], biglost.GLAD.oa,
        sf.pa[2], sf.ua[2], sf.oa, GLAD.pa[2], GLAD.ua[2], GLAD.oa)

acc = acc*100

water = data.frame(product, site, metric, acc)

## factor the sites to maintain the correct order (Reservoir, Spring-fed, Riparian, Aggregate)
water$site <- factor(water$site, levels = unique(water$site))
water$product <- factor(water$product, levels = unique(water$product))

##Grouped
ggplot(water, aes(fill = product, y = acc, x = metric)) +
  ##barplot
  geom_bar(position = 'dodge', stat = 'identity')+
  ## multipanel
  facet_wrap(~site, ncol = 2)+
  labs(fill = 'Product', y = 'Accuracy(%)', x = 'Metric') +
  scale_fill_viridis_d(direction = -1)
  