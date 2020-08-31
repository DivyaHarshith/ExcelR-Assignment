## Customer+order form

A=read.csv(file.choose())
View(A)
library(BSDA)
library(e1071)
library(nortest)
attach(A)
library(tidyr)


A2 <- table(gather(A,nation,status,1:4))
A2

## H0= Centers have same defective percentage
## H1= Centers have different defective percentage

chisq.test(A2)

## p-value = 0.2771
## Failed to reject null hypothesis
## All centers have same defective percentage