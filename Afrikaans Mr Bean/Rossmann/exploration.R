library(ggplot2)
library(randomForest)

stores <- read.csv('store.csv')
train.data <- read.csv('train.csv')

set.seed(115)
sample.stores <- sample(train.data$Store,25)
train.ss <- train.data[which(train.data$Store %in% sample.stores),]

train.ss$Date <- as.Date(train.ss$Date, format = '%Y-%m-%d')

ggplot(train.ss) + geom_line(aes(x = Date, y = Sales)) + facet_grid(Store ~.)

ggplot(train.ss[train.ss$Store == 190,])+ geom_line(aes(x = Date, y = Sales))

# exploring store 190

store190 <- merge(train.ss[train.ss$Store == 190,], stores[stores$Store == 190,])

# trim off last n days to create time series
no.of.trading.days <- length(unique(store190$DayOfWeek[store190$Open == 1]))
store190 <- store190[1:(nrow(store190) - nrow(store190) %% no.of.trading.days),][store190$Sales != 0,]

ts190 <- ts(store190$Sales, frequency = no.of.trading.days)
plot(decompose(ts190))
plot(decompose(ts190)$x - decompose(ts190)$random)
lines(decompose(ts190)$x, col = 'red')

store190$PromoMonth <- sapply(store190$Date, function(x) if (grepl(format(x, '%b'), stores$PromoInterval[stores$Store == 190])) 1 else 0)

grep(format(store190$Date, '%b')[1], as.character(stores$PromoInterval[stores$Store == 190]))



str(store190)

rf190 <- randomForest(data = store190, Sales ~ DayOfWeek + Customers + Open + Promo + StateHoliday + SchoolHoliday + PromoMonth)

pred190 <- predict(rf.190,store190)


ggplot(cbind(store190, pred190)) + geom_line(aes(x = Date, y = Sales), col = 'dodgerblue3') + geom_line(aes(x = Date, y = pred190), col = 'tomato3') 
hist(store190$Sales - pred190)
shapiro.test(store190$Sales - pred190)
#fok
