# loadclean.R - script to load clean base dataset for baseline runs, with limited
# feature engineering. Feel free to interrogate the logic of some of the decisions
# made here. We will add our wonderful, divergent and creative feature engineering
# suggestions in a future script. (Read important comment at end of script.)

# Suggestions:
# 1. We might consider using data.tables in future? Some people say it's better or
# faster in some way.

# load utility functions
source("common/utilfuncs.R")

# load data frames from training, test and store set
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)
store <- read.csv("store.csv", stringsAsFactors = FALSE)

# clean store before merge (for efficiency, don't want to propagate missing vals)
# just convert missing values (NA) to zeros for now
store[is.na(store)] <- 0

# merge sets with store data
train <- merge(train, store)
test <- merge(test, store)

# in test: store 622 has 11 NAs in Open, none fall on sundays (store closed on sundays)
# convert NA values to 1 (store is open)
test$Open[is.na(test$Open)] <- 1

# only work with observations in train set where the stores were open for now
train <- train[which(train$Open == '1'),]

# 54 observations left in train set where Sales are zero, even with customers on two
# of those days, removing them for now, but we might impute values later
train <- train[which(train$Sales != '0'),]

# separate elements of the date in train and test set
train$year <- as.integer(format(as.Date(train$Date), "%Y"))
train$month <- as.integer(format(as.Date(train$Date), "%m"))
train$day <- as.integer(format(as.Date(train$Date), "%d"))

test$year <- as.integer(format(as.Date(test$Date), "%Y"))
test$month <- as.integer(format(as.Date(test$Date), "%m"))
test$day <- as.integer(format(as.Date(test$Date), "%d"))

# NB: At this stage you need to determine what format/data types the columns need to be
# for your specific model and do a little prodding of the data in your own model script.
# Kept the whole thing generic so far for flexibility.
#
# For instance:
# randomForest & H20 rf - mlandry uses factor variables whereever possible in his
# random forests, you might want to for ex:
# levels <- unique(c(train$StoreType, test$StoreType))
# train$StoreType <- factor(train$StoreType, levels = levels)
# train$year <- factor(train$year, levels = levels)
#
# XGBoost - boosting algorithms use numeric matrices only, therefore you might want to:
# levels <- unique(c(train$StoreType, test$StoreType))
# train$StoreType <- as.integer(factor(train$StoreType, levels=levels))
# test$StoreType  <- as.integer(factor(test$StoreType,  levels=levels))
#
# Only all of the above in a looping construct to make it easy
# (check the loop in the xgboost script I'm working on)
# Remember that you need to set the levels to the factor levels present in both sets,
# hence the line: levels <- unique....