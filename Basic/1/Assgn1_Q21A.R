#Q21 A

df <- read.csv(file.choose())
df
names(df)
qqnorm(df$MPG, pch = 1 , frame = FALSE)
qqline(df$MPG,col="Blue")
