## IMDB Reviews of INCEPTION

library(rvest)
library(XML)
library(magrittr)

url <- "https://www.imdb.com/title/tt1375666/reviews?ref_=tt_ov_rt"
IMDB_inception <- NULL
for(i in 1:10){
  murl <- read_html(as.character(paste(url,i,sep = "=")))
  rev <- murl%>%
    html_nodes(".show-more__control")%>%
    html_text()
  IMDB_inception <- c(IMDB_inception,rev)
}
length(IMDB_inception)
length(unique(IMDB_inception))
IMDB_inception[duplicated(IMDB_inception)]
write.table(unique(IMDB_inception),"inception.txt",row.names = F)
inception <- readLines("inception.txt")


#####cleaning unwanted "https://"..........
df_inception <- gsub(pattern = "http.*",replacement = "",x=inception)
df_inception <- gsub("https.*","",df_inception)
df_inception <- gsub('\""','',df_inception)

library(textcat)
table(textcat(df_inception))
df_inception[which(textcat(df_inception)=="norwegian")]
consider <- c(which(textcat(df_inception)!="norwegian"))
df_inception <- df_inception[consider]
length(df_inception)


# text mining 
library(tm)
library(NLP)
stopword <- readLines(file.choose())
View(stopword)
incep_corp <- Corpus(VectorSource(df_inception))
incep_corp <- tm_map(incep_corp,removePunctuation)

### removing stop words
incep_corp <- tm_map(incep_corp,removeWords,stopword)
incep_corp <- tm_map(incep_corp,removeNumbers)
incep_corp <- tm_map(incep_corp,stripWhitespace)

# creating term document matrix
incep_tdm <- TermDocumentMatrix(incep_corp)

# converting tdm to dtm
incep_dtm <- t(incep_tdm)
rowtotal <- apply(incep_dtm,1,sum)
incep_dtm2 <- incep_dtm[rowtotal>3,]
incep_dtm2$dimnames$Terms


# LDA
library(topicmodels)
incep_LDA <- LDA(x=incep_dtm2,10)
incep_LDA_terms <- terms(incep_LDA,5)
incep_LDA_terms

topic <- terms(incep_LDA)
table <- table(names(topic),unlist(topic))
head(table)
library(cluster)

library(dendextend)
cluster <- hclust(dist(table),method = "ward.D2")
colr <-color_branches(cluster,k=3) 
plot(colr)

#NLP
library(syuzhet)
incep <- get_sentences(df_inception)
class()

#sentiment analysis
sentiments <- c("syuzhet","afinn","bing","nrc","stanford","custom")
A <- NULL
sentimentlist <- NULL
for(i in sentiments[1:4]){
  sentimentlist[[i]] <- get_sentiment(incep,method = i)
  A[[i]] <- table(get_sentiment(incep,method = i))
}
A
sentiments

#plot for NRC

plot(sentimentlist$nrc,type = "l",main = "NRC plot",xlab = "Time",ylab = "Emotion")
abline(h=0,col ="red",lwd=2)
abline(h=1,col ="blue",lwd =1)
abline(h=2,col ="yellow",lwd=1)
abline(h=4,col ="green",lwd=1)

#to extract the sentences with most negative emotions
negative <- sentimentlist$bing[which.min(sentimentlist$bing)]
incep[which(sentimentlist$bing==negative)]

#to extract the sentences with most positive emotions
positive <- sentimentlist$bing[which.max(sentimentlist$bing)]
incep[which(sentimentlist$bing==positive)]

#percent based fig
percentvalues <- get_percentage_values(sentimentlist$bing)
plot(percentvalues,type = "l",main = "percentage baced means",xlab = "TIME",ylab = "emotion value",col ="blue")
ft_values <- get_transformed_values(sentimentlist$bing,low_pass_size = 2,x_reverse_len = 100,scale_vals = T,scale_range = F)
plot(ft_values,col =ifelse(ft_values>0,"red","blue"))


#Emotion plot
nrc_data <- get_nrc_sentiment(incep)
barplot(sort(colSums(prop.table(nrc_data))),horiz = F,cex.names = 0.8,col = 1:8)



## Word cloud
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
freq <- (rowSums(as.matrix(incep_tdm)))
length(freq)
ord <- order(freq,decreasing = TRUE)
head(freq)


data_frame <- data.frame(word =names(freq),freq= freq)
windows()
wordcloud(words = data_frame$word,freq = data_frame$freq,min.freq = 3,max.words = 150,random.order = F,colors = brewer.pal(10,"Dark2"))
findFreqTerms(incep_dtm,lowfreq = 5)
findAssocs(incep_dtm,terms = "inception",corlimit = 0.2)
head(data_frame,10)

barplot(data_frame[1:10,]$freq,names.arg = data_frame[1:10,]$word,col = "navyblue")
