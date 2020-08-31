#Q21 B

wc <- read.csv(file.choose())
names(wc)
wc

#Q-Q Plot of waist

qqnorm(wc$Waist, pch = 1, frame = FALSE)
qqline(wc$Waist, col = "Blue")

#Q-Q Plot of AT
qqnorm(wc$AT, pch = 1, frame = FALSE)
qqline(wc$AT, col = "Blue")
