library(DT)
library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
library(crfsuite)
require(tm)

require(wordcloud)



require(RColorBrewer)


#With the help R code, Negative, Positive and Neutral reviews differentiated.
#The Created negative, positive and neutral are saved in the drive with the help of write.table function

negative_Reviews <- read.delim(file.choose())

View(negative_Reviews)

str(negative_Reviews)
attach(negative_Reviews)

negative_Reviews <- as.character(x)
negative_courpus <- Corpus(VectorSource(x))
print(negative_Reviews)

negative_courpus <- tm_map(negative_courpus, tolower)

inspect(negative_courpus[1:6]) 


negative_courpus <- tm_map(negative_courpus,removePunctuation)

inspect(negative_courpus[1:6])

negative_courpus <- tm_map(negative_courpus,removeNumbers)
inspect(negative_courpus[1:6])


negative_courpus <-tm_map(negative_courpus,stripWhitespace)

inspect(negative_courpus[1:6])

cleanset_negative <- tm_map(negative_courpus,removeWords, stopwords('english'))

inspect(cleanset_negative[1:6])



tdm_negative <- TermDocumentMatrix(cleanset_negative)
tdm_negative


tdm_negative <- as.matrix(tdm_negative)
tdm[1:10,1:20]



w <- rowSums(tdm_negative)  # provides the no of times a particular word has been used.
w <- subset(w, w>=10) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))





# Word Cloud :
w <- sort(rowSums(tdm_negative), decreasing = TRUE) # Sort words in decreasing order. 
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)



library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 2)





































