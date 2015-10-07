# connect to database 

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

mongo.bson.buffer.append(fields, "bug.first_priority", 1L)

mongo.bson.buffer.append(fields, "bug.first_severity", 1L)

mongo.bson.buffer.append(fields, "bug.reporter", 1L)

mongo.bson.buffer.append(fields, "bug.assigned_to", 1L)

#First comment (zero position)
mongo.bson.buffer.append(fields, "bug.long_desc.thetext", 1L)



# later should remove these attributes from testing set because not available at t0
mongo.bson.buffer.append(fields, "bug.days_resolution", 1L)

mongo.bson.buffer.append(fields, "bug.priority", 1L)

mongo.bson.buffer.append(fields, "bug.bug_severity", 1L)


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

head(gids)