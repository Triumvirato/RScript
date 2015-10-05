#  http://watson.nci.nih.gov/~sdavis/blog/rmongodb-using-R-with-mongo/

library(rmongodb)

#help("mongo.create")

#Connect to remote mongodb (macchina hostata su digitalocean)
m <- mongo.create(host = "188.166.121.194")

#Verify the connection
mongo.is.connected(m)

#Select the first document
res = mongo.find.one(m,'tesi_uniba.mongotesi')

#Print res (The result is an object of type mongo.bso)
res


#Conversion to an R list:
#resl = mongo.bson.to.list(res)
#resl
