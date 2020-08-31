library(randomForest)
library(caret)
library(psych)

fraud <- read.csv(file.choose())
View(fraud)
summary(fraud)
describe(fraud)
pairs(fraud)
str(fraud)
attach(fraud)

#converting taxable to categorical type
tax_cat <- ifelse(Taxable.Income<= 30000,"risky","good")
fraud <- data.frame(tax_cat,fraud[,-3])
table(fraud$tax_cat)

#Splitting into train and test
set.seed(100)
split <- createDataPartition(tax_cat,p=0.75,list = F)
train <- fraud[split,]
test <- fraud[-split,]

# Model building
##Model Building
forest <- randomForest(tax_cat~.,mtyr=2,data=train,importance=T)
forest

##prediction and accuracy based on train data
pred_train <- predict(forest,train)
mean(pred_train==train$tax_cat)          ##acc = 91.55%
confusionMatrix(pred_train,train$tax_cat)


##prediction and accuracy based on train data
pred_test <- predict(forest,test)
mean(pred_test==test$tax_cat)          ##acc = 76.66%
confusionMatrix(pred_test,test$tax_cat)

#visualisation
plot(forest)
legend('topright',col=2:5,colnames(forest$err.rate),fill = 2:5,cex = 0.5)

importance(forest)
varImpPlot(forest)
#most significant variable is city population

##Bagging
a <- c()
for (i in 2:10) {
  set.seed(100)
  bag <- createDataPartition(tax_cat,p=0.8,list = F)
  train_bag <- fraud[bag,]
  test_bag <- fraud[-bag,]
  bag_model <- randomForest(tax_cat~.,data = train_bag,mtyr=i,importance=TRUE)
  pred_bag <- predict(bag_model,test_bag,type="class")
  a[i-2] <- mean(pred_bag==test$tax_cat)
}
a

plot(2:9,a,xlab = "mtyr",ylab = "acc")

#Higher accuracy is for mtyr=2

##Tune Random forest
set.seed(100)
tunerf <- tuneRF(train[,-1],train[,1],improve = 0.5,stepFactor = 0.5,ntreeTry = 500,trace = TRUE,plot = TRUE)
tunerf

#choosing mtry as 3 with less OOB error

finalmodel <- randomForest(tax_cat~.,data = train,mtyr=2,importance=TRUE)
finalmodel
mean(predict(finalmodel,test)==test$tax_cat)
## accuracy is 77.33%