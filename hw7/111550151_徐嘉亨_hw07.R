library(tidyverse)
library(ggplot2)
library(psych)
library(MASS)
library(FNN)
library(pROC)
library(e1071)

red_wine <- read.csv(file = 'winequality-red.csv', header = TRUE, sep = ',')

str(red_wine)

unique(red_wine$quality)
table(red_wine$quality)

ggplot(data = red_wine) +
  geom_bar(mapping = aes(x = quality, fill=as.factor(quality)))

missing_data_summary <- data.frame(
  Feature = colnames(red_wine),
  Missing_Values = sapply(red_wine, function(x) sum(is.na(x)))
)
missing_data_summary

test = data.frame(fixed.acidity = c(8.36, 7.78, 8.17, 8.35, 8.87, 8.57),
                   volatile.acidity = c(0.8845, 0.694, 0.577, 0.497, 0.404, 0.423),
                   citric.acid = c(0.171, 0.174, 0.244, 0.274, 0.375, 0.391),
                   residual.sugar = c(2.635, 2.694, 2.529, 2.478, 2.72, 2.58),
                   chlorides = c(0.1225, 0.0907, 0.0927, 0.085, 0.0766, 0.068),
                   free.sulfur.dioxide = c(11.0, 12.26, 17.0, 15.71, 14.04, 13.28),
                   total.sulfur.dioxide = c(24.9, 36.24, 56.51, 40.87, 35.02, 33.44),
                   density = c(0.9975, 0.9965, 0.9971, 0.9966, 0.9961, 0.9952),
                   pH = c(3.4, 3.38, 3.3, 3.32, 3.29, 3.27),
                   sulphates = c(0.57, 0.596, 0.62, 0.6753, 0.74, 0.77),
                   alcohol = c(9.955, 10.265, 9.9, 10.63, 11.466, 12.1),
                   quality = as.integer(c(3, 4, 5, 6, 7, 8)))

lda_model <- lda(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,prior = c(1,1,1,1,1,1)/6,data = red_wine)
lda_model

lda_result <- predict(lda_model, test)$class
lda_result

result=table(lda_result,test[,12]);result
accuracy=sum(diag(result))/6;accuracy

red_wine$poor <- red_wine$quality <= 4
red_wine$okay <- red_wine$quality == 5 | red_wine$quality == 6
red_wine$good <- red_wine$quality >= 7
head(red_wine)

test$poor <- test$quality <= 4
test$okay <- test$quality == 5 | test$quality == 6
test$good <- test$quality >= 7

log1_good = glm(good~alcohol, data=red_wine, family=binomial(link="logit"))
log2_good = glm(good~alcohol + volatile.acidity + citric.acid + sulphates,data=red_wine,family=binomial(link="logit"))

summary(log1_good)
summary(log2_good)

Slog1_good <- pnorm(predict(log1_good))
Slog2_good <- pnorm(predict(log2_good))

roc1 <- plot.roc(red_wine$good,Slog1_good,main="",percent=TRUE, ci=TRUE, print.auc=TRUE)
roc1.se <- ci.se(roc1,specificities=seq(0,100,5))
plot(roc1.se,type="shape", col="grey")

roc2 <-plot.roc(red_wine$good,Slog2_good,main="",percent=TRUE, ci=TRUE, print.auc=TRUE)
roc2.se <- ci.se(roc2,specificities=seq(0,100,5))
plot(roc2.se,type="shape", col="blue")

prob=predict(log1_good,test,type="response")
prob
prob2=predict(log2_good,test,type="response")
prob2

tab=table(predict=prob,real=test$quality);tab
sum(diag(tab))/sum(tab)
tab2=table(predict=prob2,real=test$quality);tab2
sum(diag(tab2))/sum(tab2)

NBclass=naiveBayes(quality~.,data=red_wine)

label=predict(NBclass,test)

tab3=table(predict=label,real=test$quality);tab3
sum(diag(tab3))/sum(tab3)

class_knn10 = knn(train=red_wine[,1:11], test=red_wine[,1:11], cl = red_wine$good, k = 10) 
class_knn20 = knn(train=red_wine[,1:11], test=red_wine[,1:11], cl = red_wine$good, k = 20)
table(red_wine$good,class_knn10)
table(red_wine$good,class_knn20)

selected <- sample(1:dim(red_wine)[1], size=round(dim(red_wine)[1]*0.7))
train <- red_wine[selected,] 
test2 <- red_wine[-selected,]

fitsvml <- svm(as.factor(quality)~., kernel="linear", cross=10, data=train)
summary(fitsvml)

predsvml <- predict(fitsvml, test2)
confusion=table(test2$quality, predsvml)
sum(diag(confusion))/sum(confusion)

plot(fitsvml, train, pH~density, color.palette=terrain.colors)

fitsvmp <- svm(as.factor(quality)~., kernel="polynomial", cross=10, data=train)
summary(fitsvmp)

predsvmp <- predict(fitsvmp, test2)

confusion=table(test2$quality, predsvmp)

sum(diag(confusion))/sum(confusion)

plot(fitsvmp, train, pH~density, color.palette=terrain.colors)

fitsvmr <- svm(as.factor(quality)~., kernel="radial", cross=10, data=train)
summary(fitsvmr)

predsvmr <- predict(fitsvmr, test2)
confusion=table(test2$quality, predsvmr)
sum(diag(confusion))/sum(confusion)

fitsvms <- svm(as.factor(quality)~., kernel="sigmoid", cross=10, data=train)
summary(fitsvms)

predsvms <- predict(fitsvms, test2)
confusion=table(test2$quality, predsvms)
sum(diag(confusion))/sum(confusion)