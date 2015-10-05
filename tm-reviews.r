library(tm)

setwd('C:/Users/Marco/Documents')

reviews <- read.csv ("reviews.csv", stringsAsFactors=FALSE)

#print
str(reviews)

review_text <- paste(reviews$text, collapse=" ")

review_source <- VectorSource(review_text)

#Now we can set up the source and create a corpus
corpus <- Corpus(review_source)

#We've transformed every word to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

#We've removed all punctuation - 'fun' and 'fun!' will now be the same
corpus <- tm_map(corpus, removePunctuation)

# stripped out any extra whitespace
corpus <- tm_map(corpus, stripWhitespace)

# we removed stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

#print stopwords for test
#stopwords("english")

#Now we create the document-term matrix. we only have one document in this case, 
#our document-term matrix will only have one column.
dtm <- DocumentTermMatrix(corpus)

dtm2 <- as.matrix(dtm)

#We then take the column sums of this matrix, which will give us a named vector.
frequency <- colSums(dtm2)

#And now we can sort this vector to see the most frequently used words:
frequency <- sort(frequency, decreasing=TRUE)


head(frequency)


library(wordcloud)

words <- names(frequency)

#Let's plot the top 100 words in our cloud.
wordcloud(words[1:100], frequency[1:100])

