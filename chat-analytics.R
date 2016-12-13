#Load required packages
library(ggplot2)
library(lubridate)
library(Scale)
library(reshape2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(syuzhet) 
library(dplyr ) 

#get the data from chat (email chat for whatsapp). To give file path use \\ instead of \ eg: "C:\\sadf\\chat.txt"
texts <- readLines("chat.txt")

#let us create the corpus
docs <- Corpus(VectorSource(text))

#clean our chat data
trans <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, trans, "/")
docs <- tm_map(docs, trans, "@")
docs <- tm_map(docs, trans, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("gapp","vivek","pratyush","nus","mba","can","will"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

#create the document term matrix
dtm <- TermDocumentMatrix(docs)
mat <- as.matrix(dtm)
v <- sort(rowSums(mat),decreasing=TRUE)

#Data frame
d <- data.frame(word = names(v),freq=v)
head(data, 10)


#generate the wordcloud
set.seed(1056)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=TRUE, rot.per=.35,
          colors=brewer.pal(8, "Dark2"))


#fetch sentiment words from texts
Sentiment <- get_nrc_sentiment(texts)
head(Sentiment)
text <- cbind(texts,Sentiment)

#count the sentiment words by category
TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL
