#Q20

df <- read.csv(file.choose())
df
names(df)
mpg <- df[,2]

#p(mpg>38)
pnorm(38,mean(mpg),sd(mpg),lower.tail = FALSE)

#p(mpg>40)
pnorm(40,mean(mpg),sd(mpg))

#p(20<mpg<50)
pnorm(20,mean(mpg),sd(mpg))-pnorm(50,mean(mpg),sd(mpg))
