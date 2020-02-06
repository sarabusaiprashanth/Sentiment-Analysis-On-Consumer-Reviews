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


# Read a txt file

customer_reviews <- read.delim(file.choose())
View(customer_reviews)

str(customer_reviews)
attach(customer_reviews)


str(customer_reviews)

customer_reviews <- as.character(customer_reviews)
customer_courpus <- Corpus(VectorSource(comments))
print(customer_courpus)

customer_courpus <- tm_map(customer_courpus, tolower)
inspect(customer_courpus[1:6])

customer_courpus <- tm_map(customer_courpus,removePunctuation)
inspect(customer_courpus[1:6])

customer_courpus <- tm_map(customer_courpus,removeNumbers)
inspect(customer_courpus[1:6])

customer_courpus <-tm_map(customer_courpus,stripWhitespace)
inspect(customer_courpus[1:6])

cleanset <- tm_map(customer_courpus,removeWords, stopwords('english'))
inspect(cleanset[1:6])



tdm <- TermDocumentMatrix(cleanset)
tdm


tdm <- as.matrix(tdm)
tdm[1:10,1:20]



w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>=10) # Pull words that were used more than 10 times.
barplot(w, las = 2, col = rainbow(50))

#---------------------------------------------------------------------------------------------------------------

# Word Cloud :
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
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
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

#------------------------------------------------------------------------------------------------

#Sentiment Analysis
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)


customer_rvws <- read.delim(file.choose()) 
customer_reviews <- iconv(customer_rvws$comments)

s <- get_nrc_sentiment(customer_reviews)
s
get_nrc_sentiment('delay')
get_nrc_sentiment('ugly') 

----------------------------------------------------------------------------------------------

#Barplot
barplot(colSums(s),
        last = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Consumer reviews')


#------------------------------------------------------------------------------------------------
#Now finding the negative, positive and neutral scores of the consumer reviews. 

posText <- read.delim(file.choose(), header = FALSE, stringsAsFactors = FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n")}))
negText <- read.delim(file.choose(), header = FALSE, stringsAsFactors = FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n")}))
pos.words = c(posText, 'upgrade')
neg.words = c(negText, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')


#---------------------------------------------------------------------------------------------------------------

#customer_rvws <- readLines(file.choose())

#score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
#{
#  require(plyr);
#  require(stringr);
#  scores = laply(sentences, function(sentence, pos.words, neg.words) {
#    sentence = gsub('[^A-z ]','', sentence)
#    sentence = tolower(sentence);
#    word.list = str_split(sentence, '\\s+');
#    words = unlist(word.list);
#    pos.matches = match(words, pos.words);
#    neg.matches = match(words, neg.words);
#    pos.matches = !is.na(pos.matches);
#    neg.matches = !is.na(neg.matches);
#    score = sum(pos.matches) - sum(neg.matches);
#    return(score);
#  }, pos.words, neg.words, .progress=.progress );
#  scores.df = data.frame(score=scores, text=sentences);
#  return(scores.df);
#}

#---------------------------------------------------------------------------------------------------------------

#consumer_reviews <- readLines(file.choose())
#consumer = c(customer_reviews)
#result=score.sentiment(consumer,pos.words, neg.words)
#result

#result$score

#View(result$score)

#unique(result$score)
#hist(result$score)
#table(result$score)


pos.words = scan(file.choose(), what='character', comment.char=';')
neg.words = scan(file.choose(), what='character', comment.char=';')

neg.words = c(neg.words, 'wtf', 'fail')

#----------------------------------------------------------------------------------------------------------------------------------

#Implementing our sentiment scoring algorithm
require(plyr)
require(stringr)
require(stringi)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}



sentiment.scores= score.sentiment(customer_reviews, pos.words, neg.words, .progress='none')

score <- sentiment.scores$score
score


View(sentiment.scores)
hist(sentiment.scores$score)



library(plotly)

p <- plot_ly(x = ~score, type = "histogram")
p

---------------------------------------------------------------------------------------------------------------------

#Analysing the positive, negative and neutral words obtained from above algorithm

pos.words = scan(file.choose(), what='character', comment.char=';')
neg.words = scan(file.choose(), what='character', comment.char=';')



#Implementing our sentiment scoring algorithm
require(plyr)
require(stringr)
require(stringi)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

sentiment.scores= score.sentiment(sentiment.scores$text, pos.words, neg.words, .progress='none')
pos.mentioned = subset(sentiment.scores, score > 0)
neu.mentioned = subset(sentiment.scores, score == 0)
neg.mentioned = subset(sentiment.scores, score < 0)

N= nrow(sentiment.scores)
Npos = nrow(pos.mentioned)
Nneu = nrow(neu.mentioned)
Nneg = nrow(neg.mentioned)

dftemp=data.frame(topic=c("Positive", "Negative", "Neutral" ), 
                  number=c(Npos, Nneg, Nneu))



#Pie chart of the negative, positive and neutral reviews. 
library(plotly)
p <- plot_ly(data=dftemp, labels = ~topic, values = ~number, type = 'pie') %>%
  layout(title = 'Pie Chart of consumer reviews mentioning Positive, Negative and Neutral',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p


#-----------------------------------------------------------------------------------------------------------------------------------

#Writing the negative reviews to a text file. 

negative_Reviews = subset(sentiment.scores$text, score < 0)

View(negative_Reviews)


#All negative comments are written to a text file
write.table(negative_Reviews, "C:")

#-----------------------------------------------------------------------------------------------------------------------------

#Writing positive revuews into  a text file.
positive_reviews = subset(sentiment.scores, score > 0)

View(positive_reviews)
corpus_p = Corpus(VectorSource(positive_reviews))




#All positive reviews are written to a text file. 
write.table(positive_reviews, "C:")


#------------------------------------------------------------------------------------------------------------------------------------

#writing the neutral reviews are written to a text file. 

neutral_reviews = subset(sentiment.scores, score == 0)

View(neutral_reviews)

corpus_neutral = Corpus(VectorSource(neutral_reviews))


#All neutral reviews are written to a text file. 
write.table(neutral_reviews, "C:")


#------------------------------------------------------------------------------------------------------------------------------------------









