### LAbTAT

lab <- read.csv(file.choose())
View(lab)
attach(lab)

## Normality test
library(nortest)
shapiro.test(Laboratory.1) ##p-value = 0.5508
shapiro.test(Laboratory.2) ##p-value = 0.8637
shapiro.test(Laboratory.3) ##p-value = 0.4205
shapiro.test(Laboratory.4) ##p-value = 0.6619
### All variables are normal

#variance test
stacked <- stack(lab)
stacked
library(car)
leveneTest(stacked$values~stacked$ind,data = stacked) ##p-value = 0.05161
## Variances are equal


# One way anova 
result <- aov(values~ind,data = stacked)
summary(result)
## Reject null hypothesis


## there is difference in average TAT between different Laboratories