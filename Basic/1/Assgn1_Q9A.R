#Q9 A
#Cars speed and distance

df <- read.csv(file.choose())
names(df)
print(df)

#For Speed
sp=df['speed']
library(moments)
skewness(sp)
kurtosis(sp)

#For Distance
dt=df['dist']
skewness(dt)
kurtosis(dt)


#Draw Histogram
hist(df$speed)
hist(df$dist)

