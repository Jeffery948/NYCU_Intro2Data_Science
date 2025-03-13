library(neuralnet)
library(nnet)

red_wine <- read.csv(file = 'winequality-red.csv', header = TRUE, sep = ',')

selected <- sample(1:dim(red_wine)[1], size=round(dim(red_wine)[1]*0.7))
train <- red_wine[selected,] 
test <- red_wine[-selected,]

head(train, 10)
head(test, 10)

dim(train)
dim(test)

m1 <- min(train$quality)
m2 <- max(train$quality)

for (i in colnames(train)){
  train[i] <- scale(train[i], center = min(train[i]), scale = max(train[i]) - min(train[i]))
}
head(train)

for (i in colnames(test)){
  test[i] <- scale(test[i], center = min(test[i]), scale = max(test[i]) - min(test[i]))
}
head(test)

fitnn1 <- neuralnet(quality~., data = train, hidden = c(10), linear.output = FALSE)
plot(fitnn1, rep = 'best')

fitnn2 <- neuralnet(quality~., data = train, hidden = c(10, 3), linear.output = FALSE)
plot(fitnn2, rep = 'best')

fitnn3 <- neuralnet(quality~., data = train, hidden = c(10, 10, 3), linear.output = FALSE)
plot(fitnn3, rep = 'best')

fitnn4 <- neuralnet(quality~., data = train, hidden = c(10, 10, 3, 3), linear.output = FALSE)
plot(fitnn4, rep = 'best')

prediction <- function(fitnn)
{ 
  prednn <- compute(fitnn, test)
  ans<- prednn$net.result
  mul = function(x){
    return (x * (m2 - m1) + m1)
  }
  ans = sapply(ans, mul)
  ans = sapply(ans, round)
  test$quality <- sapply (test$quality, mul)
  confusion <- table(ans, test$quality)
  cat("Confusion table")
  print(confusion)
  accuracy <- sum(diag(confusion)) / length(ans)
  cat("Accuracy = ", accuracy, "\n")
  error = mean(ans == test$quality)
  cat("Test error = ", error, "\n")
}

prediction(fitnn1)
prediction(fitnn2)
prediction(fitnn3)
prediction(fitnn4)