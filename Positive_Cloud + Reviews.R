


#With the help R code, Negative, Positive and Neutral reviews differentiated.
#The Created negative, positive and neutral are saved in the drive with the help of write.table function

positive_reviews <- read.delim(file.choose(), header = FALSE)

View(positive_reviews)

str(positive_reviews)
attach(positive_reviews)

positive_reviews <- as.character(positive_reviews)
positive_reviews <- Corpus(VectorSource(V1))
print(positive_reviews)

positive_reviews <- tm_map(positive_reviews, tolower)

inspect(positive_reviews[1:6]) 


positive_reviews <- tm_map(positive_reviews,removePunctuation)

inspect(positive_reviews[1:6])

positive_reviews <- tm_map(positive_reviews,removeNumbers)
inspect(positive_reviews[1:6])


positive_reviews <-tm_map(positive_reviews,stripWhitespace)

inspect(positive_reviews[1:6])

cleanset_positive <- tm_map(positive_reviews,removeWords, stopwords('english'))

inspect(cleanset_positive[1:6])



tdm_positive <- TermDocumentMatrix(cleanset_positive)
tdm_positive


tdm_positive <- as.matrix(tdm_positive)
tdm_positive[1:10,1:20]



w <- rowSums(tdm_positive)  # provides the no of times a particular word has been used.
w <- subset(w, w>=10) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))


View(positive_reviews)


# Word Cloud :
w <- sort(rowSums(tdm_positive), decreasing = TRUE) # Sort words in decreasing order.
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












