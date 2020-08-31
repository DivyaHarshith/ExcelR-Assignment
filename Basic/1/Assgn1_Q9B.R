#Q9B
#SP and Weight

df <- read.csv(file.choose())
names(df)
print(df)

#For SP
SP=df['SP']
library(moments)
skewness(SP)
kurtosis(SP)

#For Weight
Weight=df['WT']
skewness(weight)
kurtosis(weight)

#Histogram
hist(df$SP)
hist(df$WT)
