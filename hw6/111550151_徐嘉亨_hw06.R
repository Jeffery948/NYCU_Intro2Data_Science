library(tidyverse)
library(psych)
library(corrplot)
library(ggplot2)
library(car)

red_wine <- read.csv(file = 'winequality-red.csv', header = TRUE, sep = ',')

class(red_wine)
str(red_wine)
head(red_wine, 10)

summary(red_wine)

describe(red_wine)

sum(!complete.cases(red_wine))

missing_data_summary <- data.frame(
  Feature = colnames(red_wine),
  Missing_Values = sapply(red_wine, function(x) sum(is.na(x)))
)
missing_data_summary

red_wine %>% cor() %>% corrplot.mixed(upper = "ellipse", tl.cex=.8, tl.pos = 'lt', number.cex = .8)

model = lm(quality ~ alcohol, data = red_wine)
summary(model)

coef(model)
confint(model)

plot(red_wine$alcohol, red_wine$quality, col = "blue", xlab = "alcohol", ylab = "quality")
abline(model, col = "red")

residuals(model)

qqnorm(residuals(model))
qqline(residuals(model),col = 'red')

new_data = data.frame(alcohol = c(9.3, 9.8, 11.3, 13.2, 12.6)) 
predict(model, new_data)

model_2nd_degree <- lm(quality ~ poly(alcohol, 2, raw = TRUE),data = red_wine)
summary(model_2nd_degree)

ggplot(red_wine, aes(alcohol, quality) ) + geom_point() + 
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) + ggtitle("2nd-degree Polynomial Regression Model")

model_3rd_degree <- lm(quality ~ poly(alcohol, 3, raw = TRUE),data = red_wine)
summary(model_3rd_degree)

ggplot(red_wine, aes(alcohol, quality) ) + geom_point() + 
  stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE)) + ggtitle("3rd-degree Polynomial Regression Model")

list('Sum of square error with intercept'=sum(residuals(model)^2),'Sum of square error with quadratic term'=sum(residuals(model_2nd_degree)^2),'Sum of square error with cubic term'=sum(residuals(model_3rd_degree)^2))

predict(model_2nd_degree, new_data)
predict(model_3rd_degree, new_data)

reg1=lm(quality ~ alcohol + volatile.acidity + citric.acid + sulphates, data = red_wine)
summary(reg1)

reg2<-step(lm(quality ~ alcohol + volatile.acidity + citric.acid + sulphates, data = red_wine) , direction = "backward")
summary(reg2)

reg3<-step(lm(quality ~ 1, data = red_wine) ,direction = "forward" , scope = ~alcohol + volatile.acidity + citric.acid + sulphates)
summary(reg3)

reg4<-step(lm(quality ~ alcohol + volatile.acidity + citric.acid + sulphates, data = red_wine) , direction = "both")
summary(reg4)

summary(reg2)$coef;summary(reg3)$coef;summary(reg4)$coef

summary(reg2)$adj.r.square;summary(reg3)$adj.r.square;summary(reg4)$adj.r.square;summary(reg1)$adj.r.square

AIC(reg2);AIC(reg3);AIC(reg4);AIC(reg1)

model1 = lm(quality ~ alcohol, data = red_wine) 

model2  = lm(quality ~ alcohol + citric.acid, data = red_wine) 

model3  = lm(quality ~ alcohol + citric.acid + sulphates, data = red_wine)

anova(model1, model2)
anova(model2, model3)

vif_values <- vif(reg1)
vif_values

loess_model <- loess(free.sulfur.dioxide ~ total.sulfur.dioxide, data = red_wine)
loess_model

series = seq(6, 289 ,by = 1)
predictions_loess <- predict(loess_model, data.frame(total.sulfur.dioxide = series))

plot(red_wine$total.sulfur.dioxide, red_wine$free.sulfur.dioxide, xlab = "total.sulfur.dioxide", ylab = "free.sulfur.dioxide", pch = 19, col = "blue", main = "Loess Regression", xlim = c(0, 175))

lines(series, predictions_loess, col = "red", lwd = 2)

loessReg1 = loess(free.sulfur.dioxide ~ total.sulfur.dioxide, span = 0.25, data = red_wine) 
loessReg2 = loess(free.sulfur.dioxide ~ total.sulfur.dioxide, span = 0.75, data = red_wine) 

plot(series, predict(loessReg1, data.frame(total.sulfur.dioxide = series)), col='blue', type ='l', lty=1, xlab="total.sulfur.dioxide", ylab = "free.sulfur.dioxide", xlim = c(0, 175), ylim = c(0, 70)) 
points(red_wine$total.sulfur.dioxide, red_wine$free.sulfur.dioxide) 
lines(series, predict(loessReg2, data.frame(total.sulfur.dioxide = series)), col='red', lty=2)
legend(130, 70, legend = c("span=0.25", "span=0.75"), col=c('blue', 'red'), lty=1:2)

ks_model <- ksmooth(red_wine$total.sulfur.dioxide, red_wine$free.sulfur.dioxide, "normal", 1)

plot(red_wine$total.sulfur.dioxide, red_wine$free.sulfur.dioxide, xlab = 'total.sulfur.dioxide', ylab = 'free.sulfur.dioxide', main="kernel regression", xlim = c(0, 175))

lines(ks_model$x, ks_model$y, type = 'l', col = 'red', lwd = 3)