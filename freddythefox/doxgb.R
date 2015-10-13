# load libraries for this model
library(xgboost)

# select the columns for prediction
predictors <- names(train)[c(1, 2, 6, 7, 9:21)]

# prod character predictors (all of them are categorical) to numeric ids
for (p in predictors) {
  if (class(train[[p]]) == "character") {
    levels <- unique(c(train[[p]], test[[p]]))
    train[[p]] <- as.integer(factor(train[[p]], levels=levels))
    test[[p]]  <- as.integer(factor(test[[p]],  levels=levels))
  }
}

# set random seed
#set.seed(2501)          # the ghost in the shell
set.seed(125)          # super-prime (5^3)

# define custom evaluation function
RMPSE <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab <- exp(as.numeric(labels)) - 1
  epreds <- exp(as.numeric(preds)) - 1
  err <- sqrt(mean((epreds / elab - 1)^2))
  return(list(metric = "RMPSE", value = err))
}

# take sample of train for evaluation function
h <- sample(nrow(train), 10000)

# partition train set
dval <- xgb.DMatrix(data.matrix(train[h, predictors]), label = log(train$Sales + 1)[h])
dtrain <- xgb.DMatrix(data.matrix(train[-h, predictors]), label = log(train$Sales + 1)[-h])

# set parameters
watchlist <- list(val = dval, train = dtrain)
param <- list(objective = "reg:linear", 
              booster = "gbtree",
              eta = 0.06,               # 0.025, 0.06, #0.01,
              max_depth = 10,           # changed from default of 8
              subsample = 0.7,         # 0.7
              colsample_bytree = 0.7    # 0.7
              # alpha = 0.0001, 
              # lambda = 1
)

bst <- xgb.train(params = param, 
                 data = dtrain,
                 nrounds = 2200,        #300, #280, #125, #250, # changed from 300
                 #verbose = 0,
                 early.stop.round = 100,
                 watchlist = watchlist,
                 maximize = FALSE,
                 feval = RMPSE
)

# plot importance of variables
im <- xgb.importance(model = bst)
xgb.plot.importance(im)

pred.bst <- exp(predict(bst, data.matrix(test[, predictors]))) - 1
test$Sales <- pred.bst

# remove sales for stores that are closed in test set
#test$Sales[test$Open == "0"] <- 0

submit.bst <- data.frame(Id = test$Id, Sales = test$Sales)
write.csv(submit.bst, file = "output/basexgb151014_0104.csv", row.names = FALSE)
