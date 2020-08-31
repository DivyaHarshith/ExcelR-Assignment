library(caret)
library(party)
library(C50)
library(gmodels)

data <- read.csv(file.choose())
View(data)
str(data)
summary(data)
pairs(data)

sort(data$Sales)
length(data$Sales)
mean(data$Sales)
# Sales may be high,medium,low
sort(data$Sales)[400/3*2]

#converting sales to categorical type
sales_cat <- ifelse(data$Sales > 8.5,"high","low")
df <- data.frame(sales_cat,data[,-1])
View(df)

#Splitting data to train and test data
set.seed(100)
split <- createDataPartition(sales_cat,p=0.75,list = F)
train <- df[split,]
test <- df[-split,]

# Building Model
# model using party package

model1 <- ctree(sales_cat~.,data = train)
pred1 <- predict(model1,test)
table(pred1,test$sales_cat)
mean(pred1==test$sales_cat)  #acc = 73.73%
plot(model1)

#model2 using c50 package

model2 <- C5.0(train[,-1],train$sales_cat,trails=100)
pred2 <- predict.C5.0(model2,test)
table(pred2,test$sales_cat)
mean(pred2==test$sales_cat)  #acc = 77.77%
plot(model2)
C5imp(model2)
CrossTable(test$sales_cat,pred2)
