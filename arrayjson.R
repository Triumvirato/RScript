#librerie
library(rmongodb)
library(plyr)
library(tm)


# connect to database 

mongo <- mongo.create(host = "188.166.121.194")


#Verify the connection
mongo.is.connected(mongo)


## create the empty data frame
#gameids = data.frame(stringsAsFactors = FALSE)

## create the namespace
DBNS = "tesi_uniba.mongotesi"

# define the query
query = mongo.bson.buffer.create()

#mongo.bson.buffer.append(query, "bug.bug_id", "45271")

# when complete, make object from buffer
query = mongo.bson.from.buffer(query)

# define the fields
fields = mongo.bson.buffer.create()
mongo.bson.buffer.append(fields,"bug.long_desc.thetext",1L)
mongo.bson.buffer.append(fields, "bug.bug_id", 1L)

#mongo.bson.buffer.finish.object(fields)
# when complete, make object from buffer
fields = mongo.bson.from.buffer(fields)

# create the cursor
cursor = mongo.find(mongo, ns = DBNS, query = query, fields = fields, limit = 2L)

## iterate over the cursor
gids = data.frame(stringsAsFactors = FALSE)

while (mongo.cursor.next(cursor)) {
  
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  
  # make it a dataframe
  tmp.df = as.data.frame(t(unlist(tmp)), stringsAsFactors = F)
  
  commentiTMP=data.frame(lapply(tmp.df["bug.long_desc.thetext":"bug.long_desc.thetext"], as.character), stringsAsFactors=FALSE)
  commenti=paste(commentiTMP,collapse=" ") 
  # bind to the master dataframe
  gids = rbind.fill(gids, tmp.df)
  
  
}

#remove creations_ts from gids
gids$bug.creation_ts = NULL

#class(gids)

#dim(gids)

#head(gids)
