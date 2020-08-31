library(psych)
library(caret)
library(class)
library(ggplot2)
zoo <- read.csv(file.choose())
View(zoo)
zoo1 <- zoo[,-1]
View(zoo1)
head(zoo1)
summary(zoo1)
describe(zoo1)
attach(zoo1)
str(zoo1)
zoo1$hair <- as.factor(zoo1$hair)
zoo1$feathers <- as.factor(zoo1$feathers)
zoo1$eggs <- as.factor(zoo1$eggs)
zoo1$milk <- as.factor(zoo1$milk)
zoo1$airborne <- as.factor(zoo1$airborne)
zoo1$aquatic <- as.factor(zoo1$aquatic)
zoo1$predator <- as.factor(zoo1$predator)
zoo1$toothed <- as.factor(zoo1$toothed)
zoo1$backbone <- as.factor(zoo1$backbone)
zoo1$breathes <- as.factor(zoo1$breathes)
zoo1$venomous <- as.factor(zoo1$venomous)
zoo1$fins <- as.factor(zoo1$fins)
zoo1$legs <- as.factor(zoo1$legs)
zoo1$tail <- as.factor(zoo1$tail)
zoo1$domestic <- as.factor(zoo1$domestic)
zoo1$catsize <- as.factor(zoo1$catsize)
zoo1$type <- as.factor(zoo1$type)
str(zoo1)
table(zoo1$type)
ggplot(zoo1)+geom_bar(mapping = aes(x=type))


#train and test
z <- createDataPartition(type,p=0.8,list = F)
train_z <- zoo1[z,]
test_z <- zoo1[-z,]


#model building
set.seed(10)
model <- knn(train_z,test_z,k=7,cl=train_z$type)
summary(model)
mean(model==test_z$type)
confusionMatrix(model,test_z$type)



train_acc <- c()
test_acc <- c()
for(i in seq(1,50,2)){
  set.seed(100)
  pred_knn_train <- knn(train_z,train_z,k=i,cl=train_z$type)
  pred_knn_test <- knn(train_z,test_z,k=i,cl=train_z$type)
  train_acc <- c(train_acc,mean(pred_knn_train==train_z$type))
  test_acc <- c(test_acc,mean(pred_knn_test==test_z$type))
}

train_acc
test_acc

par(mfrow=c(1,2))
plot(seq(1,50,2),train_acc,type = 'l',main = "train acc",col='blue')
plot(seq(1,50,2),test_acc,type = 'l',main = "test acc",col='green')

##Final Model

final_zoo <- knn(train_z,test_z,k=1,cl=train_z$type)
mean(final_zoo==test_z$type)
confusionMatrix(final_zoo,test_z$type)

##Visualisation
acc_k <- data.frame(list(train_acc=train_acc,test_acc=test_acc,k=seq(1,50,2)))
acc_k
ggplot(acc_k,aes(x=k))+
  geom_line(aes(y=train_acc,color="trn_acc"))+
  geom_line(aes(y=test_acc,color="tst_acc"))
  
  
  