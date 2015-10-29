# load libraries for h2o random forest
library(h2o)

# set random seed
#set.seed(415)           # my first seed
set.seed(2501)          # the ghost in the shell
#set.seed(125)          # super-prime (5^3)

# select the columns for prediction
predictors <- names(train)[c(1, 2, 6, 7, 9:21)]

# specify numeric factors for conversion step
#num.factors <- c("Store", "DayOfWeek", "Open", "Promo", "SchoolHoliday", "Promo2", "year", "month")
num.factors <- c("Store")

# convert character predictors (all of them are categorical) to factors
for (p in predictors) {
  if (class(train[[p]]) == "character") {
    levels <- unique(c(train[[p]], test[[p]]))
    train[[p]] <- factor(train[[p]], levels=levels)
    test[[p]]  <- factor(test[[p]],  levels=levels)
  }
}

# prod numeric factor variables into factors
for (p in num.factors) {
  levels <- unique(c(train[[p]], test[[p]]))
  train[[p]] <- factor(train[[p]], levels=levels)
  test[[p]]  <- factor(test[[p]],  levels=levels)
}

# add log sales column
train$logSales <- log(train$Sales + 1)

# Start cluster with all available threads
h2o.init(assertion = FALSE, nthreads = 1, max_mem_size = '6G')

# Load data into cluster from R
trainHex <- as.h2o(train)
testHex <- as.h2o(test)

# Set up variable to use all features other than those specified here
#features<-colnames(train)[!(colnames(train) %in% c("Id","Date","Sales","logSales","Customers"))]

# Train a random forest using all default parameters
rfHex <- h2o.randomForest(x = predictors,
                          y = "logSales",
                          ntrees = 85,
                          max_depth = 45,
                          nbins_cats = 1115, ## allow it to fit store ID
                          seed = 2501,
                          training_frame = trainHex)

summary(rfHex)

# generate predictions, as.data.frame gets them into R
pred.h2o <- as.data.frame(h2o.predict(rfHex, testHex))

# return the predictions to the original scale of the Sales data
pred.h2o <- exp(pred.h2o) - 1
summary(pred.h2o)


# generate submission file
subm.h2o <- data.frame(Id = test$Id, Sales = pred.h2o)
names(subm.h2o) <- c("Id", "Sales")
write.csv(subm.h2o, file = "output/baseh2o_rf151016_1032.csv", row.names = FALSE)
