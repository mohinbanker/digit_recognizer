library(class)
library(data.table)
library(nnet)

setwd("/home/cfpb/bankerm/Documents/personal/digit_recognition")
colclasses <- sapply(
  read.csv("train.csv", header = T, nrows = 1000),
  class)
train <- fread("train.csv", colClasses = colclasses)
colclasses <- sapply(
  read.csv("test.csv", header = T, nrows = 1000),
  class)
test <- fread("test.csv", colClasses = colclasses)

labels <- data.table(one = as.numeric(train$label == 1),
                     two = as.numeric(train$label == 2),
                     three = as.numeric(train$label == 3),
                     four = as.numeric(train$label == 4),
                     five = as.numeric(train$label == 5),
                     six = as.numeric(train$label == 6),
                     seven = as.numeric(train$label == 7),
                     eight = as.numeric(train$label == 8),
                     nine = as.numeric(train$label == 9),
                     zero = as.numeric(train$label == 0))
# labels <- as.matrix(as.factor(train$label))
labels <- as.matrix(labels)
train[, label := NULL]

train <- as.matrix(train)
test <- as.matrix(test)

m <- max(train)
train <- train/m
test <- test/m

train <- as.data.frame(train)

# pred <- knn(train, test, labels, k = 1)

nn <- nnet(labels ~ ., data = train, MaxNWts = 9999999, maxit = 750,
               size = 200, softmax = T, entropy = T)


pred <- predict(nn, test)
pred <- colnames(pred)[apply(pred, 1, which.max)]
pred <- factor(pred, levels = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "zero"))
pred <- as.numeric(pred)
pred <- replace(pred, pred == 10, 0)
submission <- data.frame(ImageId = 1:nrow(test), Label = pred)
write.csv(submission, file = "submission.csv", row.names = F)
