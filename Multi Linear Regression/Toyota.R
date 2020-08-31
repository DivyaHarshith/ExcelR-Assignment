a <- read.csv(file.choose())
View(a)

library(e1071)
corolla <- a[c('Price','Age_08_04','KM','HP','cc','Doors','Gears','Quarterly_Tax','Weight')]
View(corolla)

#1st business moment decision
summary(corolla)
str(corolla)
attach(corolla)
library(psych)
describe(corolla)

plot(Price,Age_08_04)
plot(Price,KM)
plot(Price,HP)
plot(Price,cc)
plot(Price,Doors)
plot(Price,Gears)
plot(Price,Quarterly_Tax)
plot(Price,Weight)

pairs(corolla)
cor(corolla)
library(corpcor)
cor2pcor(cor(corolla))

#multiple linear regression
model <- lm(Price~.,data = corolla)
summary(model)  #R-squared:  0.8638

library(car)
vif(model)
rmse <- mean(model$residuals^2)^.5
rmse    #1338.258
pred <- predict(model,corolla)
cor(pred,corolla$Price)   #0.9293884
influence.measures(model)
influenceIndexPlot(model,id=list(col='red',cex=2,n=10))
influencePlot(model,id=list(col='red',cex=2,n=10))
qqPlot(model)
residualPlot(model)

#Doors have more influence

modelcc <- lm(Price~cc,data = corolla)
summary(modelcc)  #R-squared:  0.01597

modeldoors <- lm(Price~Doors,data = corolla) 
summary(modeldoors)   #R-squared:  0.03435

modelccdoors <- lm(Price~cc+Doors,data = corolla)
summary(modelccdoors)  #R-squared:  0.04688


#Deleting the influenced observation
model2 <- lm(Price~.,data = corolla[-c(81,222),])
summary(model2)  #R-squared:  0.8778
rmse2 <- mean(model2$residuals^2)^.5
rmse2  #1265.72
pred2 <- predict(model2,corolla)
cor(pred2,corolla$Price)  #0.8736161
avPlots(model2)
residualPlot(model2)


# transformation model
l=log(corolla[,-1])
corolla2 <- data.frame(l,corolla$Price)

model3 <- lm(corolla.Price~.,data = corolla2)
summary(model3)  #R-squared:  0.8353
rmse3 <- mean(model3$residuals^2)^.5
pred3 <- predict(model3,corolla2)
cor(pred3,corolla2$corolla.Price)  #0.9139346
avPlots(model3)
influenceIndexPlot(model3,id=list(col='red',n=10,cex=1.5))
influencePlot(model3,id=list(col='red',n=10,cex=1.5))
qqPlot(model3)
residualPlot(model3)
vif(model3)

#log model with removing influence
model4 <- lm(corolla.Price~.,data = corolla2[-c(185,186)])
summary(model4)  #R-squared:  0.8353
rmse4 <- mean(model4$residuals^2)^.5  
rmse4  # 1471.533
pred4 <- predict(model4,corolla2)
cor(pred4,corolla2$corolla.Price)   #0.9139346


#model2 has highest R-squared value
plot(model2)
