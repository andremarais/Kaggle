library(forecast)
library(data.table)
library(mice)



# sort out refurbished stores



train <- read.csv("../train.csv", stringsAsFactors = FALSE)
train <- as.data.table(train)

Ran_Store <- 216

freq <- 6
if(Ran_Store %in% unique(train$Store[train$DayOfWeek==7 & train$Open==1])) {
  freq <- 7
} #is this a Sunday-store



train.sample<- as.data.table(train[which(train$Store==Ran_Store&train$DayOfWeek<=freq),])
train.sample$Store <- NULL
train.sample$DayOfWeek <- NULL
train.sample$Customers <- NULL
train.sample$Open <- NULL
train.sample$Promo <- NULL
train.sample$StateHoliday <- NULL
train.sample$SchoolHoliday <- NULL


train.sample <- train.sample[order(train.sample$Date),]
##03/29



ts_train<-train.sample[1:(nrow(train.sample)-48),]

ts_test<-train.sample[(nrow(train.sample)-47):nrow(train.sample),]

plot(ts_train$Sales,type='l')

ts_train$Sales[which(ts_train$Sales==0)]<-NA
imp <- mice(ts_train)
ts_train<-complete(imp)
plot(ts_train$Sales,type='l')

ts_train <- as.data.table(ts_train)
ts_train <- ts_train[6:nrow(ts_train)]
ts_train[,logSales:=log1p(Sales+1)]

ts_train$check <- c(1:6)

sc_ts <- ts(ts_train$logSales,frequency=freq)

fit <- nnetar(sc_ts)
fcast <- forecast(fit,h=100)
plot(fcast)
