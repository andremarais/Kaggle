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

#source("../common/loadclean.R")

train <- as.data.table(train)
test <- as.data.table(test)
store <- as.data.table(store)

## more care should be taken to ensure the dates of test can be projected from train
## decision trees do not project well, so you will want to have some strategy here, if using the dates
train[,Date:=as.Date(Date)]
test[,Date:=as.Date(Date)]

# seperating out the elements of the date column for the train set
train[,day:=as.integer(format(Date, "%d"))]
train[,month:=as.integer(format(Date, "%m"))]
train[,year:=as.integer(format(Date, "%y"))]
train[,Store:=as.factor(as.numeric(Store))]

test[,day:=as.integer(format(Date, "%d"))]
test[,month:=as.integer(format(Date, "%m"))]
test[,year:=as.integer(format(Date, "%y"))]
test[,Store:=as.factor(as.numeric(Store))]


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

## log transformation to not be as sensitive to high sales
## decent rule of thumb: 
##     if the data spans an order of magnitude, consider a log transform
train[,logSales:=log1p(Sales)]

#View(train[sample(nrow(train),1000),])

#View(train[which(train$Store==7),])

Store.Forecast <- train[1,]
Store.Forecast$ArimaForecast<-0
Store.Forecast<- Store.Forecast[which(is.na(Store.Forecast$text)), ]

for(z in unique(train$Store)){
  print(z)
  StoreCheck <- train[which(train$Store==z&train$DayOfWeek<7),]
  StoreCheck <- StoreCheck[order(StoreCheck$Date),]
  #StoreCheck$NewSales <- StoreCheck$Sales
  StoreCheck$Sales[which(StoreCheck$Sales==0)] <- mean(StoreCheck$Sales)
  StoreCheck$logSales<-log1p(StoreCheck$Sales)
  n<-nrow(StoreCheck)
  while(n>58){
    sc_ts <- ts(StoreCheck$logSales[1:(n-42)],frequency=6)
    fit <- nnetar(sc_ts)
    #plot(forecast.nnetar(fit,h=42,level=c(0,1)))
    f <- forecast.nnetar(fit,h=42,level=c(0,1))
    New.Forecast <-StoreCheck[(n-41):n,]
    New.Forecast$ArimaForecast <- as.numeric(f$mean)
    
    Store.Forecast <-rbind(Store.Forecast,New.Forecast)
    n <- n-42
} 
}

#Populate test set

Store.Test.Forecast <- test[1,]
Store.Test.Forecast$ArimaForecast<-0
Store.Test.Forecast<- Store.Test.Forecast[which(is.na(Store.Test.Forecast$text)), ]

for(z in unique(test$Store)){

  print(z)
  StoreCheck <- train[which(train$Store==z&train$DayOfWeek<7),]
  StoreCheck <- StoreCheck[order(StoreCheck$Date),]
  #StoreCheck$NewSales <- StoreCheck$Sales
  StoreCheck$Sales[which(StoreCheck$Sales==0)] <- mean(StoreCheck$Sales)
  StoreCheck$logSales<-log1p(StoreCheck$Sales)
  
  sc_ts <- ts(StoreCheck$logSales[1:(nrow(StoreCheck)-0)],frequency=6)
  fit <- nnetar(sc_ts)
  #plot(forecast(fit,h=42,level=c(0,1)))
  f <-forecast.nnetar(fit,h=42,level=c(0,1))
  
  Test.Forecast <- as.data.frame(test[which(test$Store==z&test$DayOfWeek<7),])
  Test.Forecast <- Test.Forecast[order(Test.Forecast$Date),]
  Test.Forecast <- Test.Forecast[, !names(Test.Forecast) %in% c('Sales','logSales')]

  
  Test.Forecast$ArimaForecast <- as.numeric(f$mean[1:41])
  Store.Test.Forecast <-rbind(Store.Test.Forecast,Test.Forecast)
} 

#write.csv(Store.Forecast,file="Store.Forecast.csv")
write.csv(Store.Test.Forecast,file="Store.Test.Forecast.csv")
#Store.Forecast <- read.csv("Store.Forecast.csv")
#Store.Forecast$Date<-as.Date(Store.Forecast$Date)
#write.csv(Store.Test.Forecast,file="Store.Test.Forecast.csv")

Store.Forecast <- Store.Forecast[,c('Store','Date','ArimaForecast')]
Store.Test.Forecast <- Store.Test.Forecast[,c('Store','Date','ArimaForecast')]

train <- merge(train,Store.Forecast,by=c('Store','Date'),all.x=TRUE)
train$ArimaForecast[which(is.na(train$ArimaForecast))] <- -99

test <- merge(test,Store.Test.Forecast,by=c('Store','Date'),all.x=TRUE)
test$ArimaForecast[which(is.na(train$ArimaForecast))] <- -99

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
                          ntrees = 100,
                          max_depth = 30,
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

