#Read data
setwd("")
mydata <- read.csv("", header = TRUE)
summary(mydata)

scaleddata<-scale(mydata)

#Set train and test groups using 80% of the sample size
set.seed(7)
smp_size <- floor(0.80 * nrow(mydata))
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]
dim(train)
dim(test)

#construct the formula
n <- names(mydata)
num <- length(n)
n <- n[-num]
x <- paste(n,collapse = '+')
x <- paste('y ~', x)
form <- as.formula(x)

#neuralnet
library(neuralnet)
nn <- neuralnet(form, data=train, hidden = c(32,10), linear.output=FALSE, err.fct = "ce", threshold=0.01)
#nn <- neuralnet(form, data=train, hidden = 32, linear.output=FALSE, err.fct = "ce", threshold=0.01)
#nn <- neuralnet(form, data=train, hidden = 64, linear.output=FALSE, err.fct = "ce", threshold=0.01)
#nn <- neuralnet(form, data=train, hidden = 5, linear.output=FALSE, err.fct = "ce", threshold=0.01)
nn$result.matrix
plot(nn)


#prediction

pred <- predict(nn, test)
table(test$y == '1', pred[ ,1] > 0.5)
confusionMatrix(table(test$y == 1, pred[,1]> 0.45))
