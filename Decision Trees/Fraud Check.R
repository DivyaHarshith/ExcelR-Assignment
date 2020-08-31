library(caret)
library(party)
library(C50)
library(gmodels)

data <- read.csv(file.choose())
View(data)
str(data)
attach(data)
summary(data)
pairs(data)
colSums(is.na(data))
hist(Taxable.Income)

# converting taxable income into categorical type
tax_cat <- ifelse(data$Taxable.Income<=30000,"risky","good")
data1 <- data.frame(tax_cat,data[,-3])
View(data1)

# Data partition
set.seed(100)
splittax <- createDataPartition(data1$tax_cat,p=0.75,list = F)
train <- data1[splittax,]
test <- data1[-splittax,]
str(train)
str(train)

#Model Building
#model using party package
model1 <- ctree(train$tax_cat~.,data=train)
pred1 <- predict(model1,test)
table(pred1,test$tax_cat)
mean(pred1==test$tax_cat)  #acc = 79.33%

#model using c50 package
model2 <- C5.0(tax_cat~.,data = train,method='class')
pred2 <- predict(model2,test)
table(pred2,test$tax_cat)
mean(pred2==test$tax_cat) #acc = 79.33%
CrossTable(pred2,test$tax_cat)
