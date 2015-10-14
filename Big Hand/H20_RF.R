# H2O Random Forest Starter Script
# Based on Ben Hamner script from Springleaf
# https://www.kaggle.com/benhamner/springleaf-marketing-response/random-forest-example
# Forked from Random Forest Example by Michael Pawlus

# Use data.table and H2O to create random forest predictions for the entire
#   training set.
# This is a starter script so it mostly uses the provided fields, as is.
# A log transform has been applied to the Sales, year and month are used,
#  and the Store ID is used as a factor.
# It does not remove the 0 readings, which may help, since we are not judged
#  on those entries. The model finds "Open" to be the most important feature,
#  which makes sense, since Sales are 0 when the store is not open. However, 
#  since observations with 0 Sales are discarded by Kaggle upon judging, it
#  may be a better strategy to remove them, as Michael's original script does.
# To make the benchmark a little more competitive, this has more and deeper 
#  trees than the original. If you want to see it run faster, you can lower those
#  settings while you work through some other parts of the model, and increase them
#  later.
# Also, the h2o.gbm() has many shared parameters, so you can try that out as well,
#  and these parameters will work (though you probably don't want depth 30 for GBM).
# And to add variety, you can try out h2o.glm(), a regularized GLM, or 
#  h2o.deeplearning(), for deep neural networks. This code will work for either with
#  the exception of the ntrees, max_depth, and nbins_cats, which are decision tree
#  parameters.
# Good luck!

library(data.table)  
library(h2o)
library(forecast)

source("../common/loadclean.R")

Store.Forecast <- read.csv("Store.Forecast.csv",stringsAsFactors = FALSE)
Store.Test.Forecast <- read.csv("Store.Test.Forecast.csv",stringsAsFactors = FALSE)

## more care should be taken to ensure the dates of test can be projected from train
## decision trees do not project well, so you will want to have some strategy here, if using the dates
train$Date=as.Date(train$Date)
test$Date=as.Date(test$Date)

Store.Forecast$Date=as.Date(Store.Forecast$Date)
Store.Test.Forecast$Date=as.Date(Store.Test.Forecast$Date)

train <- as.data.table(train)
test <- as.data.table(test)

# seperating out the elements of the date column for the train set
train[,month:=as.integer(format(Date, "%m"))]
train[,year:=as.integer(format(Date, "%y"))]
train[,Store:=as.factor(as.numeric(Store))]

test[,month:=as.integer(format(Date, "%m"))]
test[,year:=as.integer(format(Date, "%y"))]
test[,Store:=as.factor(as.numeric(Store))]

## log transformation to not be as sensitive to high sales
## decent rule of thumb: 
##     if the data spans an order of magnitude, consider a log transform
train[,logSales:=log1p(Sales)]

Store.Forecast <- Store.Forecast[,c('Store','Date','ArimaForecast')]
Store.Test.Forecast <- Store.Test.Forecast[,c('Store','Date','ArimaForecast')]

train <- merge(train,Store.Forecast,by=c('Store','Date'),all.x=TRUE)
train$ArimaForecast[which(is.na(train$ArimaForecast))] <- -99

test <- merge(test,Store.Test.Forecast,by=c('Store','Date'),all.x=TRUE)
test$ArimaForecast[which(is.na(train$ArimaForecast))] <- -99

# select the columns for prediction
predictors <- names(train)[c(1, 2, 6, 7, 9:21,23)]

# prod character predictors (all of them are categorical) to numeric ids
for (p in predictors) {
  if (class(train[[p]]) == "character") {
    levels <- unique(c(train[[p]], test[[p]]))
    train[[p]] <- as.integer(factor(train[[p]], levels=levels))
    test[[p]]  <- as.integer(factor(test[[p]],  levels=levels))
  }
}

#View(train[sample(nrow(train),1000),])

#View(train[which(train$Store==7),])

## Use H2O's random forest
## Start cluster with all available threads
h2o.init(nthreads=-1,max_mem_size='6G')
## Load data into cluster from R
trainHex<-as.h2o(train)
## Set up variable to use all features other than those specified here
features<-colnames(train)[!(colnames(train) %in% c("Id","Date","Sales","logSales","Customers"))]
## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features,
                          y="logSales", 
                          ntrees = 200,
                          max_depth = 50,
                          nbins = 500,
                          #nbins_top_level=5000,
                          binomial_double_trees = TRUE,
                          nbins_cats = 1115, ## allow it to fit store ID
                          training_frame=trainHex)

summary(rfHex)
cat("Predicting Sales\n")
## Load test data into cluster from R
testHex<-as.h2o(test)

## Get predictions out; predicts in H2O, as.data.frame gets them into R
predictions<-as.data.frame(h2o.predict(rfHex,testHex))
## Return the predictions to the original scale of the Sales data
pred <- expm1(predictions[,1])
summary(pred)
submission <- data.frame(Id=test$Id, Sales=pred)

cat("saving the submission file\n")
write.csv(submission, "h2o_rf.csv",row.names=F)

#  - - JUNKYARD



StoreCheck <- train[which(train$Store==857&train$DayOfWeek<7),]
StoreCheck <- StoreCheck[order(StoreCheck$Date),]

mean(StoreCheck$Sales)

View(StoreCheck[which(StoreCheck$Sales==0),])

StoreCheck$NewSales <- StoreCheck$Sales
StoreCheck$NewSales[which(StoreCheck$Sales==0)] <- mean(StoreCheck$Sales)

hist(StoreCheck$Sales)

StartYear <- as.numeric(format(as.Date(StoreCheck$Date[1]), "%Y")) 
EndYear <- as.numeric(format(tail(as.Date(StoreCheck$Date),1), "%Y")) 

StartWeek <- as.numeric(format(as.Date(StoreCheck$Date[1]), "%W")) 
EndWeek <- as.numeric(format(tail(as.Date(StoreCheck$Date),1), "%W")) 

