a <- read.csv(file.choose())
View(a)
str(a)

##creating dummy variables for state
a$State <- as.integer(factor(a$State,levels = c('New York','California','Florida'),labels = c(1,2,3)))
View(a)

#1st business moment decision
summary(a)
attach(a)
class(a)

library(psych)
#2nd,3rd and 4th business moment decision
describe(a)

plot(Profit,R.D.Spend,col=c('green','red'))
plot(Profit,Marketing.Spend,col=c('green','red'))
plot(Profit,Administration,col=c('green','red'))
boxplot(Profit,R.D.Spend,col=c('green','red'))
boxplot(Marketing.Spend,Administration,R.D.Spend,col=c('green','red'))
pairs(a,col="red")
cor(a)

library(corpcor)
cor2pcor(cor(a[,-c(4)]))

#multiple linear regression
model <- lm(Profit~Marketing.Spend+R.D.Spend+Administration+State,data = a)
summary(model)  #R-squared:  0.9508
rmse <- mean(model$residuals^2)^.5
rmse #8854.837
pred <- predict(model,a)
cor(pred,Profit)  #0.9750649
library(car)
vif(model)

#model influence plot
model2 <-lm(Profit~R.D.Spend+Marketing.Spend+Administration+State,data = a[-c(46,50),]) 
summary(model2)  #R-squared:  0.9635
rmse2 <- mean(model2$residuals^2)^.5
rmse2  #7160.62
pred2 <- predict(model2,a)
cor(pred2,a$Profit)  #0.974998
vif(model2)
avPlots(model2)

#Removing administration and state
modelrd <- lm(Profit~R.D.Spend+Marketing.Spend,data=a[-c(46,50),])
summary(modelrd)  #R-squared:  0.963
rmserd <- mean(modelrd$residuals^2)^.5
rmserd  #7211.373
predrd <- predict(modelrd,a)
cor(predrd,Profit)  #0.9749069


#Transformation method
log = log(a[,5])
a2 <- data.frame(log,a[,-5])
View(a)
head(a2)
model3 <- lm(log~.,data = a2)
summary(model3)   #R-squared:  0.7619
rmse3 <- mean(model3$residuals^2)^.5
rmse3  #0.2237031
pred3 <- predict(model3,a2)
cor(pred3,a2$log)  #0.8728934
vif(model3)
qqPlot(model3)


#model 2 has higher R-squared value
#model3 is good model with less rmse

plot(model2)




