library(randomForest)
library(caret)
library(psych)
company <- read.csv(file.choose())
View(company)
summary(company)
describe(company)
pairs(company)
str(company)
attach(company)


## Creating categorical data on sales variable
length(Sales)
sort(Sales)
mean(Sales)
sort(Sales)[400/3*2]

sales_cat <- ifelse(Sales > 8.5,"high","low")
company <- data.frame(sales_cat,company[,-1])
View(company)

set.seed(100)
split <- createDataPartition(sales_cat,p=0.75,list = F)
train <- company[split,]
test <- company[-split,]

##Model Building
companyforest <- randomForest(sales_cat~.,ntree=500,mtyr=3,data=train,importance=T)
companyforest

##prediction and accuracy based on train data
pred_train <- predict(companyforest,train)
mean(pred_train==train$sales_cat)          ##acc = 100%
confusionMatrix(pred_train,train$sales_cat)

##prediction and accuracy
pred_test <- predict(companyforest,test)
mean(pred_test==test$sales_cat)            ##acc = 79.83%
confusionMatrix(pred_test,test$sales_cat)

#visualisation
plot(companyforest)
legend('topright',col=2:11,colnames(companyforest$err.rate),fill = 2:11,cex = 0.5)

## variable importance
importance(companyforest)
varImpPlot(companyforest)
# Price is significant variable

##Bagging
a <- c()
for (i in 3:10) {
  set.seed(100)
  bag <- createDataPartition(sales_cat,p=0.8,list = F)
  train_bag <- company[bag,]
  test_bag <- company[-bag,]
  bag_model <- randomForest(sales_cat~.,data = train_bag,mtyr=i,importance=TRUE)
  pred_bag <- predict(bag_model,test_bag,type="class")
  a[i-2] <- mean(pred_bag==test$sales_cat)
}
a
plot(3:10,a,xlab = "mtyr",ylab = "acc")
#highest accuracy for mtyr 3


##Tune Random forest
set.seed(100)
tunerf <- tuneRF(train[,-1],train[,1],improve = 0.5,stepFactor = 0.5)
tunerf

#choosing mtry as 3 with less OOB error

finalmodel <- randomForest(sales_cat~.,data = train,mtyr=3,importance=TRUE)
finalmodel
mean(predict(finalmodel,test)==test$sales_cat)
## accuracy is 81.81%


