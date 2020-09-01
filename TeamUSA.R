
# Setting the working directory for R to pick the file from
setwd("C://Mydata//dellstudio//US_Stuff//wrk-study//work//applications//Team USA oLY")

#Picking up the required files
FB <- read.csv("FB.csv", stringsAsFactors = FALSE)

TWT1 <- read.csv("TWT.csv", stringsAsFactors = FALSE)

INSTA2 <- read.csv("INSTA.csv", stringsAsFactors = FALSE)

Newsarticles <- read.csv("TeamUSAonly.csv", stringsAsFactors = FALSE)

#intalling package for text analysis
install.packages("tm")

#Loading the required packages
library(tm)

#creating a copy of a particular dataset 
TWT1.1 = TWT1

#creating a subset of TWT to get that data where type = photo and retweet > 33 (average)

subTWT1.1 = subset(TWT1.1, Type == "photo" & Retweets > 33)

#Combining all the messages from top posts 
message_text <- paste(subTWT1.1$Message, collapse = " ")

#Seetting up source and corpus
message_Source <- VectorSource(message_text)
Corpus <- Corpus(message_Source)

#cleaning
corpus <- tm_map(Corpus, content_transformer(tolower))
corpus <- tm_map(Corpus, removePunctuation)
corpus <- tm_map(Corpus, stripWhitespace)
corpus <- tm_map(Corpus, removeWords, stopwords("english"))
  
#making a document - term matrix
dtm <- DocumentTermMatrix(corpus)
dtm2<- as.matrix(dtm) 

#finding the most frequent terms
frequency <- colSums(dtm2)

#to find the structure of the document created "frequency"
str(frequency)

#to check the words with its frequency
frequency

#sorting the frequencies 
frequency <- sort(frequency, decreasing = TRUE)

install.packages('wordcloud')

library(wordcloud)
 
#to convert the words in the required format for the wrd cloud
words <- names(frequency)

#to check the top words in the "words' vector
head(words)

wordcloud(words[1:100], frequency[1:100])

#Doing this to round off the time to nearest hour (Not too helpful - not used)
## POSIX*t objects need both date and time specified
## Here, the particular date doesn't matter -- just that there is one.

FB$dayparts <- strptime(paste("2001-01-01", FB$X), format = "%Y - %m - %d %H:%M")

## Use round.Date to round, then format to format
fb$dayparts <- ifelse(fb$X == " ", 0, format(round(FB$dayparts, units="hours"), format="%H:%M"))

## Doing a wrdcld for Insta Data
#creating a copy of a particular dataset 
INSTA1 = INSTA2

#creating a subset of TWT to get that data where type = photo and retweet > 33 (average)

subINSTA1 = subset(INSTA1, Type == "photo" & Total_Engagement > 8256)

#Combining all the messages from top posts 
message_text <- paste(subINSTA1$Description, collapse = " ")

#Seetting up source and corpus
message_Source <- VectorSource(message_text)
Corpus <- Corpus(message_Source)

#cleaning
corpus <- tm_map(Corpus, content_transformer(tolower))
corpus <- tm_map(Corpus, removePunctuation)
corpus <- tm_map(Corpus, stripWhitespace)
corpus <- tm_map(Corpus, removeWords, stopwords("english"))

#making a document - term matrix
dtm <- DocumentTermMatrix(corpus)
dtm2<- as.matrix(dtm) 

#finding the most frequent terms
frequency <- colSums(dtm2)

#to find the structure of the document created "frequency"
str(frequency)

#to check the words with its frequency
frequency

#sorting the frequencies 
frequency <- sort(frequency, decreasing = TRUE)

install.packages('wordcloud')

library(wordcloud)

#to convert the words in the required format for the wrd cloud
words <- names(frequency)

#to check the top words in the "words' vector
head(words)

wordcloud(words[1:94], frequency[1:94])

## Doing a wrdcld for FB Data
#creating a copy of a particular dataset 
FB1 = FB

#creating a subset of TWT to get that data where type = photo and retweet > 33 (average)

subFB1 = subset(FB1, Type == "photo" & Shares > 14)

#Combining all the messages from top posts 
message_text <- paste(subFB1$Message, collapse = " ")

#Seetting up source and corpus
message_Source <- VectorSource(message_text)
Corpus <- Corpus(message_Source)

#cleaning
corpus <- tm_map(Corpus, content_transformer(tolower))
corpus <- tm_map(Corpus, removePunctuation)
corpus <- tm_map(Corpus, stripWhitespace)
corpus <- tm_map(Corpus, removeWords, stopwords("english"))

#making a document - term matrix
dtm <- DocumentTermMatrix(corpus)
dtm2<- as.matrix(dtm) 

#finding the most frequent terms
frequency <- colSums(dtm2)

#to find the structure of the document created "frequency"
str(frequency)

#to check the words with its frequency
frequency

#sorting the frequencies 
frequency <- sort(frequency, decreasing = TRUE)

install.packages('wordcloud')

library(wordcloud)

#to convert the words in the required format for the wrd cloud
words <- names(frequency)

#to check the top words in the "words' vector
head(words)

wordcloud(words[1:100], frequency[1:100], colors=brewer.pal(6,"Dark2"))

## Doing a wrdcld for News articles
#creating a copy of a particular dataset 
Newsarticles1 = Newsarticles

#creating a subset of TWT to get that data where type = photo and retweet > 33 (average)

subNewsarticles1 = subset(Newsarticles1, Pageviews > 1738)

#Combining all the messages from top posts 
message_text <- paste(subNewsarticles1$Page_Title, collapse = " ")

#Seetting up source and corpus
message_Source <- VectorSource(message_text)
Corpus <- Corpus(message_Source)

#cleaning
corpus <- tm_map(Corpus, content_transformer(tolower))
corpus <- tm_map(Corpus, removePunctuation)
corpus <- tm_map(Corpus, stripWhitespace)
corpus <- tm_map(Corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeNumbers)


##Installing A package for stemming the doccument
install.packages("SnowballC")
library(SnowballC)

corpus <- tm_map(Corpus, stemDocument)



#making a document - term matrix
dtm <- DocumentTermMatrix(corpus)
dtm2<- as.matrix(dtm) 

#finding the most frequent terms
frequency <- colSums(dtm2)

#to find the structure of the document created "frequency"
str(frequency)

#to check the words with its frequency
frequency

#sorting the frequencies 
frequency <- sort(frequency, decreasing = TRUE)

install.packages('wordcloud')

library(wordcloud)

#to convert the words in the required format for the wrd cloud
words <- names(frequency)

#to check the top words in the "words' vector
head(words)
tail(words)


wordcloud(words[1:100], frequency[1:100],colors=brewer.pal(6,"Dark2"))



