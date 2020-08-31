##Q7

df <- read.csv(file.choose())
#Points
df[,2]
#Mean
Points.mean <- mean(df[,2])
Points.mean
#Median
Points.median <- median(df[,2])
Points.median
#Mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
Points.mode <- mode(df[,2])
Points.mode
#Variance
var(df[,2])
#SD
Points.Standard_Deviation <- sd(df[,2])
Points.Standard_Deviation
#Range
Points.Range <- (max(df[,2])-min(df[,2]))
Points.Range



df <- read.csv(file.choose())
#Score
df[,3]
#Mean
Score.mean <- mean(df[,3])
Score.mean
#Median
Score.median <- median(df[,3])
Score.median
#Mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
Score.mode <- mode(df[,3])
Score.mode
#Variance
var(df[,3])
#SD
Score.Standard_Deviation <- sd(df[,3])
Score.Standard_Deviation
#Range
Score.Range <- (max(df[,3])-min(df[,3]))
Score.Range



df <- read.csv(file.choose())
#weigh
df[,4]
#Mean
weigh.mean <- mean(df[,4])
weigh.mean
#Median
weigh.median <- median(df[,4])
weigh.median
#Mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
weigh.mode <- mode(df[,4])
weigh.mode
#Variance
var(df[,4])
#SD
weigh.Standard_Deviation <- sd(df[,4])
weigh.Standard_Deviation
#Range
weigh.Range <- (max(df[,4])-min(df[,4]))
weigh.Range

