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

cat("reading the train and test data (with data.table) \n")
train <- read.csv("../train.csv", stringsAsFactors = T)
test <- read.csv("../test.csv", stringsAsFactors = T)
store <- read.csv("../store.csv", stringsAsFactors = T)

train <- as.data.table(train)
test <- as.data.table(test)
store <- as.data.table(store)

## more care should be taken to ensure the dates of test can be projected from train
## decision trees do not project well, so you will want to have some strategy here, if using the dates
train[,Date:=as.Date(Date)]
test[,Date:=as.Date(Date)]

# seperating out the elements of the date column for the train set
train[,month:=as.integer(format(Date, "%m"))]
train[,year:=as.integer(format(Date, "%y"))]
train[,year:=as.integer(format(Date, "%d"))]

test[,month:=as.integer(format(Date, "%m"))]
test[,year:=as.integer(format(Date, "%y"))]
test[,year:=as.integer(format(Date, "%d"))]


# clean store before merge (for efficiency, don't want to propagate missing vals)
# just convert missing values (NA) to zeros for now
store[is.na(store)] <- 0

test$Open[is.na(test$Open)] <- 1

# Mark refurbished stores
RefurbishedStores <- as.numeric(subset(data.frame(table(train$Store)), Freq<900)$Var1)
#cat("Number of refurbished stores:")
#length(RefurbishedStores)

store$RefurbishedStore <- as.numeric(store$Store %in% RefurbishedStores)
train$PostRefurb <- ifelse(train$Store %in% RefurbishedStores & train$Date > as.Date("2014-12-31"),1,0)
test$PostRefurb <- ifelse(test$Store %in% RefurbishedStores & test$Date > as.Date("2014-12-31"),1,0)

# impute Competition Values 
store$CompetitionOpenSinceYear[is.na(store$CompetitionOpenSinceYear)] <- 1990 # Dealing with NA and outlayers
store$CompetitionOpenSinceMonth[is.na(store$CompetitionOpenSinceMonth)] <- 1 # Dealing with NA
store$CompetitionDistance[is.na(store$CompetitionDistance)] <- 75000 # Dealing with NA

store$CompetitionStrength <- cut(store$CompetitionDistance, breaks=c(0, 1500, 6000, 12000, 20000, Inf), labels=FALSE) # 15 min, 1/2/3 hours (or walking and 10/20/30 min driving)

store$Promo2SinceDate <- as.Date(paste(store$Promo2SinceYear, store$Promo2SinceWeek, 1, sep=" "), format = "%Y %U %u")
store$CompetitionOpenSinceDate <- as.Date(paste(store$CompetitionOpenSinceYear, store$CompetitionOpenSinceMonth, 1, sep=" "), format = "%Y %m %d")

store$SundayStore <- as.numeric(store$Store %in% unique(train$Store[train$DayOfWeek==7 & train$Open==1])) #is this a Sunday-store

store[,StoreType:=as.factor(StoreType)]
store[,Assortment:=as.factor(Assortment)]

#cat("store data column names and details\n")
#summary(store)

# merge in store information
train <- merge(train,store,by="Store")
test <- merge(test,store,by="Store")

#is.Promo.Month <- format(train$Date, "%b") %in% unlist(strsplit(as.character(train$PromoInterval), split=","))
#train$Promo2Refresh <- as.numeric((train$Date > train$Promo2SinceDate) & is.Promo.Month)
#train$Promo2Refresh[is.na(train$Promo2Refresh)] <- 0

#is.Promo.Month <- format(test$Date, "%b") %in% unlist(strsplit(as.character(test$PromoInterval), split=","))
#test$Promo2Refresh <- as.numeric((test$Date > test$Promo2SinceDate) & is.Promo.Month)
#test$Promo2Refresh[is.na(test$Promo2Refresh)] <- 0

#CompetitionEffect = 30
#train$CompetitionEntrance <- 0
#train$CompetitionEntrance <- as.numeric((train$Date > train$CompetitionOpenSinceDate) & (train$Date < train$CompetitionOpenSinceDate + CompetitionEffect))
#train$CompetitionStrength <- ifelse(train$Date > train$CompetitionOpenSinceDate, train$CompetitionStrength, train$CompetitionStrength + 1)
#train$CompetitionStrength <- as.factor(train$CompetitionStrength)

#test$CompetitionEntrance <- 0
#test$CompetitionEntrance <- as.numeric((test$Date > test$CompetitionOpenSinceDate) & (test$Date < test$CompetitionOpenSinceDate + CompetitionEffect))
#test$CompetitionStrength <- ifelse(test$Date > test$CompetitionOpenSinceDate, test$CompetitionStrength, test$CompetitionStrength + 1)
#test$CompetitionStrength <- as.factor(test$CompetitionStrength)

#cat("train data column names and details\n")
#summary(train)
#cat("test data column names and details\n")
#summary(test)

train[,Store:=as.factor(as.numeric(Store))]
test[,Store:=as.factor(as.numeric(Store))]

train[,DayOfWeek:=as.factor(as.numeric(DayOfWeek))]
test[,DayOfWeek:=as.factor(as.numeric(DayOfWeek))]

train[,StateHoliday:=as.factor(StateHoliday)]
test[,StateHoliday:=as.factor(StateHoliday)]

train[,SchoolHoliday:=as.factor(as.numeric(SchoolHoliday))]
test[,SchoolHoliday:=as.factor(as.numeric(SchoolHoliday))]

train <- train[Sales > 0,]  ## We are not judged on 0 sales records in test set
## See Scripts discussion from 10/8 for more explanation.

# impute 11 days open for one store in the test dataset
test$Open[is.na(test$Open)] <- 1

## log transformation to not be as sensitive to high sales
## decent rule of thumb: 
##     if the data spans an order of magnitude, consider a log transform
train[,logSales:=log1p(Sales)]

Store.Forecast <- read.csv("Store.Forecast.csv",stringsAsFactors = F)
Store.Test.Forecast <- read.csv("Store.Test.Forecast.csv",stringsAsFactors = F)

Store.Forecast <- Store.Forecast[,c('Store','Date','ArimaForecast')]
Store.Test.Forecast <- Store.Test.Forecast[,c('Store','Date','ArimaForecast')]

Store.Forecast <- as.data.table(Store.Forecast)
Store.Test.Forecast <- as.data.table(Store.Test.Forecast)

## more care should be taken to ensure the dates of test can be projected from train
## decision trees do not project well, so you will want to have some strategy here, if using the dates
Store.Forecast[,Date:=as.Date(Date)]
Store.Test.Forecast[,Date:=as.Date(Date)]

train <- merge(train,Store.Forecast,by=c('Store','Date'),all.x=TRUE)
train$ArimaForecast[which(is.na(train$ArimaForecast))] <- -99

test <- merge(test,Store.Test.Forecast,by=c('Store','Date'),all.x=TRUE)
test$ArimaForecast[which(is.na(train$ArimaForecast))] <- -99

# select the columns for prediction
predictors <- names(train)[c(1, 2, 6, 7, 9:26,28)]

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
#features<-colnames(train)[!(colnames(train) %in% c("Id","Date","Sales","logSales","Customers"))]
features<-predictors
## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features,
                          y="logSales", 
                          ntrees = 200,
                          max_depth = 40,
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
write.csv(submission, paste("h2o_rf_BigHand_",format(Sys.Date(),"%Y%m%d"),format(Sys.time(),"%H%m"),".csv",sep=""),row.names=F)

#  - - JUNKYARD
