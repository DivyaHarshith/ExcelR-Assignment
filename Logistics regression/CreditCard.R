install.packages("AER")
data <- read.csv(file.choose())
attach(data)

data = data[c("card","reports","age","income","share","expenditure","owner","selfemp","dependents","months","majorcards","active")]
View(data)

data[,1:12] <- sapply(data[,1:12],as.character)
n <- nrow(data)
for (i in 1:n) {if(data$card[i]=="yes"){data$card[i] <- 1} else{data$card[i] <- 0}}
for (i in 1:n) {if(data$owner[i]=="yes"){data$owner[i] <- 1}else{data$owner[i] <- 0}}
for (i in 1:n) {if(data$selfemp[i]=="yes"){data$selfemp[i] <- 1}else{data$selfemp[i] <- 0}}

data[,1:12] <- sapply(data[,1:12],as.numeric)

library(caTools)
set.seed(123)
split = sample.split(card,SplitRatio = 0.8)
train = subset(data,split==TRUE)
test = subset(data,split == FALSE)

model <- glm(card~.,data = train,family = binomial)
summary(model)

pred <- predict(model,test,type = "response")
View(pred)

pred <- ifelse(pred>.5,1,0)
View(pred)

NROW(pred)

Accuracy <- table(test$card,pred)
Accuracy

acc <- sum(diag(Accuracy)/sum(Accuracy))
acc

## Accuracy is 97%