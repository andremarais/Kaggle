library(xgboost)
library('ProgGUIinR')


#my favorite seed^^

cat("reading the train and test data\n")
train<-read.csv("../train.csv", stringsAsFactors = F)
test<-read.csv("../test.csv", stringsAsFactors = F)
store<-read.csv("../store.csv", stringsAsFactors = F)

#---
## more care should be taken to ensure the dates of test can be projected from train
cat("converting dates\n")
train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)

cat("separating out the elements of the date column\n")
train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer (format(train$Date, "%d"))
train$week.of.month <- as.integer (week.of.month(train$year,train$month,train$day))

test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer (format(test$Date, "%d"))
test$week.of.month <- as.integer (week.of.month(test$year,test$month,test$day))


cat("mark refurbished stores\n")
RefurbishedStores <- as.numeric(subset(data.frame(table(train$Store)), Freq<900)$Var1)
#cat("Number of refurbished stores:")
#length(RefurbishedStores)

store$RefurbishedStore <- as.numeric(store$Store %in% RefurbishedStores)
train$PostRefurb <- ifelse(train$Store %in% RefurbishedStores & train$Date > as.Date("2014-12-31"),1,0)
test$PostRefurb <- ifelse(test$Store %in% RefurbishedStores & test$Date > as.Date("2014-12-31"),1,0)

cat("impute competition values\n")
store$CompetitionOpenSinceYear[is.na(store$CompetitionOpenSinceYear)] <- 1990 # Dealing with NA and outlayers
store$CompetitionOpenSinceMonth[is.na(store$CompetitionOpenSinceMonth)] <- 1 # Dealing with NA
store$CompetitionDistance[is.na(store$CompetitionDistance)] <- 75000 # Dealing with NA

#cat("cut competitiondistance and convert promo2sincedate, create sundaystore and convert some factors\n")
#store$CompetitionStrength <- cut(store$CompetitionDistance, breaks=c(0, 1500, 6000, 12000, 20000, Inf), labels=FALSE) # 15 min, 1/2/3 hours (or walking and 10/20/30 min driving)

#store$Promo2SinceDate <- as.Date(paste(store$Promo2SinceYear, store$Promo2SinceWeek, 1, sep=" "), format = "%Y %U %u")
store$CompetitionOpenSinceDate <- as.Date(paste(store$CompetitionOpenSinceYear, store$CompetitionOpenSinceMonth, 1, sep=" "), format = "%Y %m %d")

#store$SundayStore <- as.numeric(store$Store %in% unique(train$Store[train$DayOfWeek==7 & train$Open==1])) #is this a Sunday-store

store$StoreType <- as.factor(store$StoreType)
store$Assortment <- as.factor(store$Assortment)

#cat("store data column names and details\n")
#summary(store)

cat("..merging...\n")
train <- merge(train,store,by="Store")
test <- merge(test,store,by="Store")

#cat("dealing with promo2refresh\n")
#is.Promo.Month <- format(train$Date, "%b") %in% unlist(strsplit(as.character(train$PromoInterval), split=","))
#train$Promo2Refresh <- as.numeric((train$Date > train$Promo2SinceDate) & is.Promo.Month)
#train$Promo2Refresh[is.na(train$Promo2Refresh)] <- 0

#is.Promo.Month <- format(test$Date, "%b") %in% unlist(strsplit(as.character(test$PromoInterval), split=","))
#test$Promo2Refresh <- as.numeric((test$Date > test$Promo2SinceDate) & is.Promo.Month)
#test$Promo2Refresh[is.na(test$Promo2Refresh)] <- 0

#cat("adding competition effect flag\n")
CompetitionEffect = 30
train$CompetitionEntrance <- 0
train$CompetitionEntrance <- as.numeric((train$Date > train$CompetitionOpenSinceDate) & (train$Date < train$CompetitionOpenSinceDate + CompetitionEffect))
#train$CompetitionStrength <- ifelse(train$Date > train$CompetitionOpenSinceDate, train$CompetitionStrength, train$CompetitionStrength + 1)
#train$CompetitionStrength <- as.factor(train$CompetitionStrength)

test$CompetitionEntrance <- 0
test$CompetitionEntrance <- as.numeric((test$Date > test$CompetitionOpenSinceDate) & (test$Date < test$CompetitionOpenSinceDate + CompetitionEffect))
#test$CompetitionStrength <- ifelse(test$Date > test$CompetitionOpenSinceDate, test$CompetitionStrength, test$CompetitionStrength + 1)
#test$CompetitionStrength <- as.factor(test$CompetitionStrength)

#cat("train data column names and details\n")
#summary(train)
#cat("test data column names and details\n")
#summary(test)
cat("force some more factors\n")
#train$Store <- as.factor(as.numeric(train$Store))
#test$Store <- as.factor(as.numeric(test$Store))

#train$DayOfWeek <- as.factor(as.numeric(train$DayOfWeek))
#test$DayOfWeek <- as.factor(as.numeric(test$DayOfWeek))

train$StateHoliday <- as.factor(train$StateHoliday)
test$StateHoliday <- as.factor(test$StateHoliday)

#train$SchoolHoliday <- as.factor(as.numeric(train$SchoolHoliday))
#test$SchoolHoliday <- as.factor(as.numeric(test$SchoolHoliday))

cat("limit train to sales >0 and create logtransform\n")
train <- train[train$Sales > 0,]  ## We are not judged on 0 sales records in test set
## See Scripts discussion from 10/8 for more explanation.

# impute 11 days open for one store in the test dataset
test$Open[is.na(test$Open)] <- 1

## log transformation to not be as sensitive to high sales
## decent rule of thumb: 
##     if the data spans an order of magnitude, consider a log transform
#train$logSales <- log1p(train$Sales)
train$Promo2SinceWeek[is.na(train$Promo2SinceWeek)] <- 1
train$Promo2SinceYear[is.na(train$Promo2SinceYear)] <- 2016

train$PromoInterval[is.na(train$PromoInterval)] <- 0
train$PromoInterval <- as.factor(train$PromoInterval)

test$Promo2SinceWeek[is.na(test$Promo2SinceWeek)] <- 1
test$Promo2SinceYear[is.na(test$Promo2SinceYear)] <- 2016

test$PromoInterval[is.na(test$PromoInterval)] <- 0
test$PromoInterval <- as.factor(test$PromoInterval)

# There are some NAs in the integer columns so conversion to zero
#cat("any more NAs?\n")
#length(train[is.na(train)])  
#length(test[is.na(test)])

#cat("train data column names and details\n")
#names(train)
#str(train)
#summary(train)
#cat("test data column names and details\n")
#names(test)
#str(test)
#summary(test)

col2remove <- c("Id", "Date", "Sales","Customers", 
                "Promo2SinceDate", # no need
                "CompetitionOpenSinceDate", #no need
                "StateHoliday", # pretty useless
                "CompetitionStrength", #useless
                "SundayStore", # useless
                "RefurbishedStore", # useless
                #"PostRefurb", an OK feature
                "Promo2Refresh"#, #useless
                #"CompetitionEntrance" #useless
)

feature.names <- colnames(train)[!(colnames(train) %in% col2remove)]

cat("Feature Names\n")
feature.names

# good function, but I dont need it now
#cat("assuming text variables are categorical & replacing them with numeric ids\n")
#for (f in feature.names) {
#  if (class(train[[f]])=="character") {
#    levels <- unique(c(train[[f]], test[[f]]))
#    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
#    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
#  }
#}

cat("cut the features\n")
tra<-train[,feature.names]

RMPSE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMPSE", value = err))
}
nrow(train)
#set.seed(777)
h<-sample(nrow(train),50000)

dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=log(train$Sales+1)[h])
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(train$Sales+1)[-h])
watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "reg:linear", 
                booster = "gbtree",
                eta                 = 0.25, # 0.06, #0.01,
                max_depth           = 8, #changed from default of 8
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.7 # 0.7
                
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 200, #700, #280, #125, #250, # changed from 700
                    verbose             = 1,
                    early.stop.round    = 30,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    feval=RMPSE
)
pred1 <- exp(predict(clf, data.matrix(test[,feature.names])))-1
library(readr)
submission <- data.frame(Id=test$Id, Sales=pred1)
cat("saving the submission file\n")
write_csv(submission, "rf1.csv")

