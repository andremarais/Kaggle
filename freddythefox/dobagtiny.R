# dobagtiny.R - take the mean of tiny bag of xgb runs
bag1 <- read.csv("freddythefox/bag/bag1.csv")
bag2 <- read.csv("freddythefox/bag/bag2.csv")
bag3 <- read.csv("freddythefox/bag/bag3.csv")

tinybag <- data.frame(Id = bag1$Id, bag1 = bag1$Sales, bag2 = bag2$Sales, bag3 = bag3$Sales)
meanbag <- apply(tinybag[,2:4], 1, mean)

submit.bag <- data.frame(Id = tinybag$Id, Sales = meanbag)
write.csv(submit.bag, file = "output/basebagxgb151014_1315.csv", row.names = FALSE)
