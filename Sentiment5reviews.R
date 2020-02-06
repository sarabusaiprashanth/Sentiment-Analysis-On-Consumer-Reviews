library(SentimentAnalysis)

#Analyzing sentimenr and score of 5 comments from the consumer reviews.


sentiment <- analyzeSentiment("Yeah, this was a great soccer game of the German team!")

convertToBinaryResponse(sentiment)$SentimentGI


# Create a vector of strings
documents <- c("Warm, friendly, courteous & helpful nature of the person. Listening with intent to understand, help & resolve issue customer was facing
                problem sorted.",
               "INCREASE NUMBER OF OUTLETS WHERE CARD IS ENTERTAINED",
               "THE PERSON TALKED TO ME VERY RESPECTFULLY AND TO THE POINT. HE WAS VERY RESPONSIVE OF MY QUERRIES.",
               "Better response from customer service is excepted.",
               "Courteous and polite consultant",
               "He was helpful and did tell me the best way to use my points.")


#[1] 0.3125000 0.2000000 0.5000000 0.6000000 0.6666667 0.5000000


sentiment <- analyzeSentiment(documents)


# Extract dictionary-based sentiment according to the QDAP dictionary
sentiment$SentimentQDAP


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------


#sentiment analysis - Emotion

library(syuzhet)

sentence <- "Warm, friendly, courteous & helpful nature of the person. Listening with intent to understand, help & resolve issue customer was facing
             problem sorted."


sentence2 <- "INCREASE NUMBER OF OUTLETS WHERE CARD IS ENTERTAINED"
sentence3 <- "THE PERSON TALKED TO ME VERY RESPECTFULLY AND TO THE POINT. HE WAS VERY RESPONSIVE OF MY QUERRIES."
sentence4 <- "Better response from customer service is excepted."
sentence5 <- "He was helpful and did tell me the best way to use my points."



new_sentence <- as.character(strsplit(sentence, " "))
get_nrc_sentiment(new_sentence)

new_sentence <- as.character(strsplit(sentence2, " "))
get_nrc_sentiment(new_sentence)

new_sentence <- as.character(strsplit(sentence3, " "))
get_nrc_sentiment(new_sentence)

new_sentence <- as.character(strsplit(sentence4, " "))
get_nrc_sentiment(new_sentence)

new_sentence <- as.character(strsplit(sentence5, " "))
get_nrc_sentiment(new_sentence)

#anger anticipation disgust fear joy sadness surprise trust negative positive
#1     0            1       0    1   2       1        0     2        1        4

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Read a txt file

customer_reviews <- read.delim(file.choose())
View(customer_reviews)

str(customer_reviews)
attach(customer_reviews)

#------------------------------------------------


str(customer_reviews)
customer_reviews <- as.character(customer_reviews)
customer_courpus <- Corpus(VectorSource(comments))
print(customer_courpus)

#-------------------------------------------------


customer_courpus <- tm_map(customer_courpus, tolower)

inspect(customer_courpus[1:6]) 

#--------------------------------------------------

customer_courpus <- tm_map(customer_courpus,removePunctuation)
inspect(customer_courpus[1:6])

customer_courpus <- tm_map(customer_courpus,removeNumbers)
inspect(customer_courpus[1:6])

customer_courpus <-tm_map(customer_courpus,stripWhitespace)
inspect(customer_courpus[1:6])

cleanset <- tm_map(customer_courpus,removeWords, stopwords('english'))
inspect(cleanset[1:6])

#-----------------------------------------------------

tdm <- TermDocumentMatrix(cleanset)
tdm

#------------------------------------------------------

#carryout sentiment mining using the get_nrc_sentiment()function #log the findings under a variable result
result <- get_nrc_sentiment(as.character(cleanset))


#------------------------------------------------------


#change result from a list to a data frame and transpose it 
result1<-data.frame(t(result))

#------------------------------------------------------


#rowSums computes column sums across rows for each level of a #grouping variable.
new_result <- data.frame(rowSums(result1))

#------------------------------------------------------

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL


#------------------------------------------------------

#plot the first 5 rows,the distinct emotions
qplot(sentiment, data=new_result[1:5,], weight=count, geom="bar",fill=sentiment)+ggtitle("TedTalk Sentiments")


#-------------------------------------------------------

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("TedTalk Sentiments")


#------------------------------------------------------------------------











































