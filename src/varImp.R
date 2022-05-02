library(ggplot2)
library(dplyr)
library(stringr)

setwd("~/BSU/diss/SF_MS/data/")

## VI files
water = read.csv('VarImportance_SF/waterExplain.csv')
mesic = read.csv('VarImportance_SF/mesicExplain.csv')

#water
watervals = strsplit(as.character(water$importance), ",")
waterraw.df = as.data.frame(watervals)
colnames(waterraw.df) = c('rawvals')

spl <-strsplit(as.character(waterraw.df$rawvals), "=")
water.df = data.frame(V1= sapply(spl, "[", 1), V2 = sapply(spl, "[", 2))
## replace curly brace chars from the EE output
water.df[] <- lapply(water.df, gsub, pattern='}', replacement='')
water.df[] <- lapply(water.df, gsub, pattern='\\{', replacement='')

#mesic
mesicvals = strsplit(as.character(mesic$importance), ",")
mesicraw.df = as.data.frame(mesicvals)
colnames(mesicraw.df) = c('rawvals')

spl <-strsplit(as.character(mesicraw.df$rawvals), "=")
mesic.df = data.frame(V1= sapply(spl, "[", 1), V2 = sapply(spl, "[", 2))
## replace curly brace chars from the EE output
mesic.df[] <- lapply(mesic.df, gsub, pattern='}', replacement='')
mesic.df[] <- lapply(mesic.df, gsub, pattern='\\{', replacement ='')


## water
## Sort the values
water.df$V2 = round(as.numeric(water.df$V2), 2)
water.sort = water.df%>%arrange(desc(V2))
water.sort = head(water.sort, 10)%>%arrange(V2)
colnames(water.sort) = c('Covariate', 'MDG')
water.sort
water.sort$Covariate <- factor(water.sort$Covariate, levels = water.sort$Covariate)
water.plot = ggplot(data = water.sort, aes( x = Covariate, y = MDG)) +
  ##barplot
  geom_bar( stat = 'identity', width =0.5) + coord_flip()+
  theme(axis.text = element_text(size = 12)) 

water.plot


## Mesic
## Sort the values
mesic.df$V2 = round(as.numeric(mesic.df$V2), 2)
mesic.sort = mesic.df%>%arrange(desc(V2))
mesic.sort = head(mesic.sort, 10)%>%arrange(V2)
colnames(mesic.sort) = c('Covariate', 'MDG')
mesic.sort
mesic.sort$Covariate <- factor(mesic.sort$Covariate, levels = mesic.sort$Covariate)
mesic.plot = ggplot(data = mesic.sort, aes( x = Covariate, y = MDG)) +
  ##barplot
  geom_bar( stat = 'identity', width =0.5) + coord_flip()+
  theme(axis.text = element_text(size = 12))   

mesic.plot