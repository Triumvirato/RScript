#librerie
library(rmongodb)
library(plyr)
library(tm)
library(rJava)

java.stem.completion <- function(corpus, dictionary, num.threads = 1)
{
  .jinit(classpath = ".", force.init = TRUE)
  stem.completion.obj <- .jnew("stem_completion/StemCompletion",
                               as.character(corpus), as.character(dictionary),
                               as.integer(num.threads))
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
  #trainindex <- sample(index, trunc(length(index)/2))
  
  smp_size = floor(trn_size * nrow(gids))
  
  trainindex <- sample(seq_len(nrow(gids)), size = smp_size)
  
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

###
### corpusPreProcess does the text preprocessing on a corpus
###
corpusPreProcess = function(corpus) {
  
  # remove URLs (versione 1)
  #removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  #corpus <- tm_map(corpus, content_transformer(removeURL))
  
  # remove URLs
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
  
  # transform every word to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # remove all punctuation - 'fun' and 'fun!' will now be the same
  corpus <- tm_map(corpus, removePunctuation)
  
  # strip out any extra whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  
  # remove all strings longer than 15 characters
  remove.long.terms = function(x, max.length)
  {
    return(PlainTextDocument(paste(sapply(strsplit(as.character(x), " "),
                                          function(y)
                                          {
                                            ifelse(nchar(y) > max.length, "", y)
                                          }
    ), collapse = " ")))
  }
  
  corpus <- tm_map(corpus, remove.long.terms,max.length=15)
  
  # remove stop words
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  # remove all strings which start with numbers
  #corpus <- tm_map(corpus, toSpace, "[[:digit:]]+") <-------------------------------
  # remove all strings which start with non alfanumeric characters
  corpus <- tm_map(corpus, toSpace, "[^[:alnum:]]+")
  
  # remove all strings shorter than 3 characters
  remove.short.terms = function(x, min.length)
  {
    return(PlainTextDocument(paste(sapply(strsplit(as.character(x), " "),
                                          function(y)
                                          {
                                            ifelse(nchar(y) < min.length, "", y)
                                          }
    ), collapse = " ")))
  }
  
  corpus <- tm_map(corpus, remove.short.terms,min.length=3)
<<<<<<< HEAD
=======
  
  # copy the corpus for next completion
  corpus.copy = corpus
>>>>>>> 4ca8ca0c45e419a447a00fb412bee6ec5517ceb5
  
  
  # remove again all punctuation
  corpus <- tm_map(corpus, removePunctuation)
  
  # strip out again any extra whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  
  # do the stemming of corpus
  corpus = tm_map(corpus,stemDocument,language="english")
  
  # do the completion of corpus with most frequent term
  #corpus = tm_map(corpus, content_transformer(stemCompletion), dictionary = corpus.copy)
  
            #codice per la Completion alternativa (prof Bilancia) da rivedere
            #L <- length(corpus)
            #charList <- lapply(1:L, function(x) as.character(corpus[[x]]))
            #strSplitList <- lapply(charList, function(x) strsplit(x, " ")[[1]])
            #processedStrSplitList <- lapply(strSplitList, function(x){
            #  x <- x[(which(x != ""))] })
            
            #i <<- 0
            #stemCompletionList <- lapply(processedStrSplitList,  function(x){
            #  x <- stemCompletion(x, dictionary=corpus.copy, type="prevalent")
            #  names(x) <- NULL
            #  x <- paste(x, collapse = " ")
            #  print(i <<- i + 1)
            #  print(x)	
            #})
  
  
            ### creazione e salvataggio del corpus pre-processato
            #corpus <- VCorpus(VectorSource(as.character(stemCompletionList)))
            #writeLines(as.character(corpus[[1]]))
  
            # creo dizionario
            # copy the corpus for next completion
            corpus.copy = corpus
            dict <- DocumentTermMatrix(corpus)
            corpus = java.stem.completion(corpus, corpus.copy, num.threads = 1)
  
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

# mongo.bson.buffer.append(query, "bug.bug_id", "45271")

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
cursor = mongo.find(mongo, ns = DBNS, query = query, fields = fields, limit = 100L)

# iterate over the cursor
gids = data.frame(stringsAsFactors = FALSE)

while (mongo.cursor.next(cursor)) {
  
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
  
  # bind to the master dataframe
  gids = rbind.fill(gids, tmp.df)

  
}

#remove creations_ts from gids
gids$bug.creation_ts = NULL

#class(gids)

#dim(gids)

#head(gids)



# append bugs id to row name
for (i in 1:nrow(gids)){
  row.names(gids)[i] = gids[i,1]
}

# divide data in two df, one for training and one for testing ( trn_size set the boundary )
splits <- splitdf(gids, trn_size=0.8, seed=204)

# it returns a list - two data frames called trainset and testset
str(splits)

lapply(splits, nrow)

# view the first few columns in each data frame
lapply(splits, head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset


# Filter unusable attributes from testing set ( because not available at t0)
testing$bug.priority = NULL
testing$bug.bug_severity = NULL
testing$comments = NULL

# Delete bug.bug_id because isn't useful
training$bug.bug_id = NULL
testing$bug.bug_id = NULL

# Create corpora
corpus_training = Corpus(VectorSource(training),readerControl = list(language="eng"))
corpus_testing = VCorpus(DataframeSource(testing),readerControl = list(language="eng"))

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

#Create term document matrix for training set
dtm_training = TermDocumentMatrix(corpus_training)
inspect(dtm_training)

#Create term document matrix for testing set
dtm_testing = TermDocumentMatrix(corpus_testing)
inspect(dtm_testing)
