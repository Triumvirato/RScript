library(rmongodb)
library(plyr)
library(tm)



# splitdf function will return a list of training and testing sets
# trn_size: percentuale training set
splitdf <- function(dataframe, trn_size=0.8, seed=NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  #trainindex <- sample(index, trunc(length(index)/2))
  
  smp_size = floor(trn_size * nrow(gids))
  
  trainindex <- sample(seq_len(nrow(gids)), size = smp_size)
  
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
  
}




mongo <- mongo.create(host = "188.166.121.194")

#Verify the connection
mongo.is.connected(mongo)




## create the empty data frame
gameids = data.frame(stringsAsFactors = FALSE)

## create the namespace
DBNS = "tesi_uniba.mongotesi"

# define the query
query = mongo.bson.buffer.create()

#mongo.bson.buffer.append(query, "bug.bug_id", "45271")

# when complete, make object from buffer
query = mongo.bson.from.buffer(query)

# define the fields
fields = mongo.bson.buffer.create()

#mongo.bson.buffer.append(fields, "bug.bug_id", 1L)

mongo.bson.buffer.append(fields, "bug.product", 1L)

mongo.bson.buffer.append(fields, "bug.component", 1L)

mongo.bson.buffer.append(fields, "bug.creation_ts", 1L)

mongo.bson.buffer.append(fields, "bug.short_desc", 1L)

mongo.bson.buffer.append(fields, "bug.priority", 1L)

mongo.bson.buffer.append(fields, "bug.bug_severity", 1L)

mongo.bson.buffer.append(fields, "bug.reporter", 1L)

mongo.bson.buffer.append(fields, "bug.assigned_to", 1L)

#Per il training
mongo.bson.buffer.append(fields, "bug.first_priority", 1L)

mongo.bson.buffer.append(fields, "bug.first_severity", 1L)




mongo.bson.buffer.append(fields, "_id", 0L)


# when complete, make object from buffer
fields = mongo.bson.from.buffer(fields)

# create the cursor
cursor = mongo.find(mongo, ns = DBNS, query = query, fields = fields, limit = 30L)

## iterate over the cursor
gids = data.frame(stringsAsFactors = FALSE)

while (mongo.cursor.next(cursor)) {
  
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  
  # make it a dataframe
  tmp.df = as.data.frame(t(unlist(tmp)), stringsAsFactors = F)
  
  # concatenate attribute name to attribute value
  tmp.df$bug.priority = paste("priority", tmp.df$bug.priority ,sep = "_")
  
  tmp.df$bug.bug_severity = paste("bug_severity", tmp.df$bug.bug_severity ,sep = "_")

  #Convert string to a date value
  tmp.df$bug.creation_ts = as.Date(tmp.df$bug.creation_ts, format="%Y-%m-%d")
  
  #Select only month
  tmp.df$bug.creation_month = format(tmp.df$bug.creation_ts,"month_%B")
  
  #Select only year 
  tmp.df$bug.creation_year = format(tmp.df$bug.creation_ts,"year_%Y")
  
  
  # bind to the master dataframe
  gids = rbind.fill(gids, tmp.df)

  
}

#class(gids)

#dim(gids)

#head(gids)



#Divide data

#apply the function
splits <- splitdf(gids, trn_size=0.5, seed=808)

#it returns a list - two data frames called trainset and testset
str(splits)

# there are 75 observations in each data frame
lapply(splits,nrow)

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

#end Divide data


  #Filter data testing
  testing$bug.priority = NULL
  testing$bug.bug_severity = NULL

  

corpus = VCorpus(DataframeSource(gids),readerControl = list(language="eng"))

#We've transformed every word to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

# We've removed all punctuation - 'fun' and 'fun!' will now be the same
corpus <- tm_map(corpus, removePunctuation)

# stripped out any extra whitespace
corpus <- tm_map(corpus, stripWhitespace)

# we removed stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# we copy the corpus for next completion
corpus.copy = corpus

# we do the stemming of corpus
corpus.temp = tm_map(corpus,stemDocument,language="english")

# we do completion
corpus.final <- tm_map(corpus.temp, content_transformer(stemCompletion), dictionary = corpus.copy)

dtm = TermDocumentMatrix(corpus)
inspect(dtm)