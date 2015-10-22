#librerie

library(rmongodb)
library(plyr)
library(tm)
library(rJava)

###
### java.stem.completion will do the completion of corpus using a java class
###

java.stem.completion <- function(corpus, dictionary)
{
  # initialize JVM
  .jinit(classpath = ".", force.init = TRUE)
  
  # transform corpus into a array
  c=sapply(corpus, `[`, "content")
  
  # now create an array where every component is a concatenate string of a bug's words
  c_length=length(c)
  starting_element = unlist(c[[1]])
  corpus_array = paste(starting_element, collapse = " ")
  for(i in 2:c_length)
  {
    temp = unlist(c[[i]])
    temp = paste(temp, collapse = " ")
    corpus_array = append(corpus_array,temp)
  }
  
  stem.completion.obj <- .jnew("stem_completion/StemCompletion",
                               corpus_array, as.character(dictionary))
  result <- VCorpus(VectorSource(stem.completion.obj$stemCompletion()))
  return(result)
}


###
### splitdf function will return a list of training and testing sets
### trn_size: percentuale training set
###
splitdf <- function(dataframe, trn_size=0.8, seed=NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  
  smp_size = floor(trn_size * nrow(gids))
  
  trainindex <- sample(seq_len(nrow(gids)), size = smp_size)
  
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

###
### corpusPreProcess does the text preprocessing on a corpus
###
corpusPreProcess = function(corpus)
{
  
  # create a meta pattern
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  # transform every word to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # remove URLs
  corpus <- tm_map(corpus, toSpace, "[[:punct:]]?http[s]?://[[:graph:]]*[[:punct:]]?")
  
  # remove all strings which are shorter than 3
  # non funziona bene
  #corpus <- tm_map(corpus, toSpace, "\\b[a-z]\\w{1,3}\\b")
  
  # remove all punctuation - 'fun' and 'fun!' will now be the same
  corpus <- tm_map(corpus, removePunctuation)
  
  # remove stop words
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  # strip out any extra whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  
  # remove all strings which are not strings or number
  corpus <- tm_map(corpus, toSpace, "[^a-z0-9]+")
  
  # remove all strings which start with numbers
  corpus <- tm_map(corpus, toSpace, "[0-9]+[a-z]+")
  
  # remove all strings which are only numbers
  corpus <- tm_map(corpus, toSpace, "\\b[0-9]+\\b")
  
  # remove all strings which are longer than 15 characters
  corpus <- tm_map(corpus, toSpace, "\\b[a-z]\\w{15,}\\b")
  
  # strip out again any extra whitespace
  corpus = tm_map(corpus, stripWhitespace)
  
  # copy the corpus for next completion
  corpus.copy = corpus
  
  # do the stemming of corpus
  corpus = tm_map(corpus,stemDocument,language="english")
  
  # strip out again any extra whitespace
  corpus = tm_map(corpus, stripWhitespace)
  
  # create dictionary ordering words by frequency
  dtm <- DocumentTermMatrix(corpus.copy)
  dict <- dtm$dimnames$Terms[as.integer(names(sort(tapply(dtm$v,
                                                          dtm$j,sum),
                                                         decreasing=TRUE)))]
  corpus = java.stem.completion(corpus, dict)
  
  return (corpus)

}


# connect to database
mongo <- mongo.create(host = "188.166.121.194")

# Verify the connection
mongo.is.connected(mongo)

# create the namespace
DBNS = "tesi_uniba.mongotesi"

# define the query
query = mongo.bson.buffer.create()

# when complete, make object from buffer
query = mongo.bson.from.buffer(query)

# define the fields
fields = mongo.bson.buffer.create()

mongo.bson.buffer.append(fields, "bug.bug_id", 1L)

mongo.bson.buffer.append(fields, "bug.product", 1L)

mongo.bson.buffer.append(fields, "bug.component", 1L)

mongo.bson.buffer.append(fields, "bug.creation_ts", 1L)

mongo.bson.buffer.append(fields, "bug.short_desc", 1L)

mongo.bson.buffer.append(fields, "bug.first_priority", 1L)

mongo.bson.buffer.append(fields, "bug.first_severity", 1L)

mongo.bson.buffer.append(fields, "bug.reporter", 1L)

mongo.bson.buffer.append(fields, "bug.assigned_to", 1L)

# later should remove these attributes from testing set because not available at t0
mongo.bson.buffer.append(fields, "bug.days_resolution", 1L)

mongo.bson.buffer.append(fields, "bug.priority", 1L)

mongo.bson.buffer.append(fields, "bug.bug_severity", 1L)

# All Comments
mongo.bson.buffer.append(fields, "bug.long_desc.thetext", 1L)

mongo.bson.buffer.append(fields, "_id", 0L)

# when complete, make object from buffer
fields = mongo.bson.from.buffer(fields)

# create the cursor
# insert limit = xxxL as parameter for limiting the number of result set
cursor = mongo.find(mongo, ns = DBNS, query = query, fields = fields)

# iterate over the cursor
gids = data.frame(stringsAsFactors = FALSE)

while (mongo.cursor.next(cursor))
{
  
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  
  # make it a dataframe
  tmp.df = as.data.frame(t(unlist(tmp)), stringsAsFactors = F)
  
  # concatenate attribute name to attribute value
  tmp.df$bug.priority = paste("priority", tmp.df$bug.priority ,sep = "_")
  
  tmp.df$bug.bug_severity = paste("bug_severity", tmp.df$bug.bug_severity ,sep = "_")
  
  tmp.df$bug.days_resolution = paste("res_days", tmp.df$bug.days_resolution ,sep = "_")

  # Convert string to a date value
  tmp.df$bug.creation_ts = as.Date(tmp.df$bug.creation_ts, format="%Y-%m-%d")
  
  # Select only month
  tmp.df$bug.creation_month = format(tmp.df$bug.creation_ts,"month_%B")
  
  # Select only year 
  tmp.df$bug.creation_year = format(tmp.df$bug.creation_ts,"year_%Y")
  
  
  col_primo = which(colnames(tmp.df)=="bug.long_desc.thetext")[1]
  col_iniziale = which(colnames(tmp.df)=="bug.long_desc.thetext")[2]
  col_finale = which(colnames(tmp.df)=="bug.long_desc.thetext")[length(which(colnames(tmp.df)=="bug.long_desc.thetext"))]
  
  if(! (is.na(col_primo) || is.na(col_iniziale) || is.na(col_finale) ))
  {
    
  
    #versione 1: collassa i commenti in un unico campo comments
    
    commentiTMP=data.frame(lapply(tmp.df[col_iniziale:col_finale], as.character), stringsAsFactors=FALSE)
    commenti=paste(commentiTMP,collapse=" ")
    
    colnames(tmp.df)[col_primo] <- "first_comment"
    
    #remove columns
    for(i in col_iniziale:col_finale)
    {
      tmp.df["bug.long_desc.thetext"]=NULL
    }
    ##tmp.df<- subset(tmp.df, select=-(col_finale-col_iniziale))
    tmp.df$comments = commenti
    
    
    # Versione 2: utilizza una colonna per ogni commento
    #k=1
    #for(i in col_iniziale:col_finale)
    #{
    #  #rinonimo le colonne dei commenti come 'comment k'
    #  colnames(tmp.df)[i] = paste("comment",k)
    #  k=k+1
    #}
  
  }
  
  # bind to the master dataframe
  gids = rbind.fill(gids, tmp.df)
}

# append bugs id to row name
for (i in 1:nrow(gids)){
  row.names(gids)[i] = gids[i,1]
}

# remove no more useful columns from gids
gids$bug.creation_ts = NULL
gids$bug.long_desc.thetext = NULL
gids$bug.bug_id = NULL

# divide data in two df, one for training and one for testing ( trn_size set the boundary )
splits <- splitdf(gids, trn_size=0.8, seed=204)

# it returns a list - two data frames called trainset and testset
str(splits)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

# Filter unusable attributes from testing set ( because not available at t0)
testing$bug.priority = NULL
testing$bug.bug_severity = NULL
testing$comments = NULL


# Create corpora
corpus_training = VCorpus(DataframeSource(training))
corpus_testing = VCorpus(DataframeSource(testing))

# Preproces corpora
corpus_training = corpusPreProcess(corpus_training)
corpus_testing = corpusPreProcess(corpus_testing)

# change corpus id with the id of bug
for (i in 1:length(corpus_training)) {
  meta(corpus_training[[i]], tag="id") <- row.names(training)[i]
}

for (i in 1:length(corpus_testing)) {
  meta(corpus_testing[[i]], tag="id") <- row.names(testing)[i]
}

# Create term document matrix for training set
dtm_training = TermDocumentMatrix(corpus_training)

# Create term document matrix for testing set
dtm_testing = TermDocumentMatrix(corpus_testing)

# Write dtms on csv file
write.csv(inspect(dtm_training),"dtm_training.csv")
write.csv(inspect(dtm_testing),"dtm_testing.csv")