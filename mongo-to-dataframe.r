library(rmongodb)

mongo <- mongo.create(host = "188.166.121.194")

#Verify the connection
mongo.is.connected(mongo)


library(plyr)

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

mongo.bson.buffer.append(fields, "bug.bug_id", 1L)

mongo.bson.buffer.append(fields, "bug.product", 1L)

mongo.bson.buffer.append(fields, "bug.short_desc", 1L)

mongo.bson.buffer.append(fields, "_id", 0L)


# when complete, make object from buffer
fields = mongo.bson.from.buffer(fields)

# create the cursor
cursor = mongo.find(mongo, ns = DBNS, query = query, fields = fields, limit = 100L)

## iterate over the cursor
gids = data.frame(stringsAsFactors = FALSE)

while (mongo.cursor.next(cursor)) {
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  # make it a dataframe
  tmp.df = as.data.frame(t(unlist(tmp)), stringsAsFactors = F)
  # bind to the master dataframe
  gids = rbind.fill(gids, tmp.df)
}

class(gids)

dim(gids)

head(gids)


review_source <- VectorSource(gids)

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


filtro = subset(corpus, TRUE, select="bug.short_desc")

#inspect(filtro)

#tdm= TermDocumentMatrix(filtro)

#inspect(tdm)



combined <- c(corpus, filtro)

tdm= TermDocumentMatrix(combined)

inspect(tdm)


