
library(randomForest)

setwd('~/BSU/diss/SF_MS/data')

# Load Dataset
data <- read.csv('training_merged40kWet.csv')
# remove the columns of non-predictors
data.train = subset(data, select = -c(class, .geo, system.index, random))
# tune mtry
data.tune = tuneRF(data.train, as.factor(data$class), stepFactor = 1.5)
