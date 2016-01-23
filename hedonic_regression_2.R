require(irlba)
require(tm)
require(wordcloud)
require(stringr)
require(slam)

require(glmnet)
require(caret)
require(matrixStats)
require(dplyr)



setwd ("/work/")
#comment.char command avoids treating # as comment.  eg 123 main street #3
LAcounty <- read.table("TEST_LA_2015.txt",sep="\t", header=FALSE, fill=TRUE, comment.char="", quote="")
dim(LAcounty)
cols <- c("ParcelID","zip5", "closedate","closeprice","n_bathroom","n_bedroom","live_sqrft","SS_id","REO_id","FC_id","remarks","yearbuilt","closeyear")
colnames(LAcounty) <- cols

# keep distressed sales only
#short sale, foreclosure, REO
dist_sales <-filter(LAcounty,grepl("Y",SS_id) | grepl("F",FC_id) | grepl("P", FC_id) | grepl("REO", REO_id) | grepl("Short", REO_id))

# will create indicator variables for zip code
# keep observations from zip codes with at least 40 observations
chk <-select(dist_sales, ParcelID, zip5)
zipcount <- chk %>% group_by(zip5) %>% summarise(num_obs_by_zip=n())
dist_sales2 <- dist_sales %>% left_join(zipcount, by="zip5")
chk2 <- filter(dist_sales2, num_obs_by_zip==1)
dist_sales3 <-filter(dist_sales2, num_obs_by_zip > 40)

#some listings have no comments
#create indicator variable to test if lack of comments
#affect price.
#other research found that lack of comments for distressed sales
#are associated with properties that are flipped at higher price
#shortly after
dist_sales3$no_comm <-with(dist_sales3, ifelse(dist_sales3$remarks=='',1,0))




########### text analysis###############

#replace punctuation characters with white space.  removePunctuation command seems to join words and not create whitespace
dist_sales3$remarksb <-str_replace_all(dist_sales3$remarks, '[[:punct:]]',' ')

comments <-subset(dist_sales3, select="remarksb")

#create corpus for use with tm package commands
corp <-Corpus(DataframeSource(comments))


########## clean text data, using tm package ###################
#inspect(corp[1:2])

corp <- tm_map(corp, content_transformer(tolower))

# create stopword list
# add extra extra stop words to standard english stopword list
myStopwords <- c(stopwords('english'), "room", "home", "family", "bed", "bedroom", "bath","bedrooms" )
# remove  stopwords from list here
myStopwords <- setdiff(myStopwords, c("big"))
corp <- tm_map(corp, removeWords, myStopwords)

corp <- tm_map(corp, removeNumbers) # derive numeric variables from other fields in dataset
corp<- tm_map(corp, removePunctuation)  # will convert 3.5ba to 35ba
corp <- tm_map(corp, stemDocument)
#corp <- tm_map(corp, stripWhitespace)


#### convert corpus to document term matrix ##################

#use bigrams instead of bag of words
options(mc.cores=1)
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
DTM <- DocumentTermMatrix(corp, control = list(tokenize = BigramTokenizer))
dim(DTM)
DTM2 <- removeSparseTerms(DTM, .995)
dim(DTM2)

inspect(DTM2[1:5,200:205])

termMat <- as.matrix(DTM2)
termDF <- as.data.frame(termMat)

ordmDTM <-termMat[,order(colSums(termMat),decreasing=T)]
dim(ordmDTM)
# keep 3k most common terms.  this can be adjusted
#tmpmat <-ordmDTM[,1:3000]
tmpmat <-termDF[,colSums(termDF) > 10]
dim(tmpmat)
tmpDF <- data.frame(tmpmat)

##############################################################
## other variables for regression

morevars <-subset(dist_sales3, select=c("n_bathroom","n_bedroom", "live_sqrft", "closeyear", "no_comm","SS_id", "FC_id", "REO_id"))
morevars$rownumber = 1:nrow(morevars)
tmpDF$rownumber = 1:nrow(tmpDF)
xvars0 <-merge(morevars,tmpDF, by="rownumber")

#### zip dummies
morevarsb <- subset(dist_sales3, select="zip5")
zipf <-factor(morevarsb$zip5)
dummies = model.matrix(~zipf)
zipdum<-data.frame(dummies)
chk2<- filter(zipdum, zipf91724==1)
zipdum$rownumber = 1:nrow(morevars)
xvars <-merge(xvars0,zipdum, by="rownumber")


xvars$missbath <-with(xvars, ifelse(is.na(xvars$n_bathroom),1,0))
xvars$missbed <-with(xvars, ifelse(is.na(xvars$n_bedroom),1,0))
xvars$misssqrft <-with(xvars, ifelse(is.na(xvars$live_sqrft),1,0))


xvars$n_bathroom <-with(xvars, ifelse(is.na(xvars$n_bathroom),0,xvars$n_bathroom))
xvars$n_bedroom <-with(xvars, ifelse(is.na(xvars$n_bedroom),0,xvars$n_bedroom))
xvars$live_sqrft <-with(xvars, ifelse(is.na(xvars$live_sqrft),0,xvars$live_sqrft))
xvars$yr2014 <-with(xvars, ifelse(xvars$closeyear==2014,1,0))
xvars$yr2015 <-with(xvars, ifelse(xvars$closeyear==2015,1,0))

#dummy variables for type of distressed sale
xvars$SS <- -with(xvars, ifelse(xvars$SS_id=="Y",1,0))
xvars$FC1 <- -with(xvars, ifelse(xvars$FC_id=="F",1,0))
xvars$FC2 <- -with(xvars, ifelse(xvars$FC_id=="P",1,0))
xvars$REO1 <- -with(xvars, ifelse(xvars$REO_id=="REO",1,0))

xvarsz <-subset(xvars, select=-c(rownumber,closeyear,X.Intercept., FC_id, SS_id, REO_id, zipf91724))

t1 <-subset(dist_sales3, select="closeprice")
t1$lnprice <- log(t1$closeprice)

yvar  <- subset(t1, select="lnprice")

Mxvar =as.matrix(xvarsz)
Myvar=as.matrix(yvar)


# alpha =1 is LASSO
cv <- cv.glmnet(Mxvar,Myvar,alpha=.99, nfold=10) 
#coef(cv, s = "lambda.min")


matcoeff <- as.matrix(coef(cv, s = "lambda.min"))
#head(matcoeff, n=10)
#str(matcoeff)


dfcoeff <- data.frame(matcoeff)
dfcoeff <- cbind(Variables = rownames(dfcoeff), dfcoeff)

#head(dfcoeff, n=10)
#str(dfcoeff)
cols <- c("Variable","Value")
colnames(dfcoeff) <- cols


dfcoeff <-filter(dfcoeff, !grepl("zipf",Variable)) 
dfcoeff <- subset(dfcoeff,Variable !="(Intercept)")
dfcoeff <- subset(dfcoeff,Variable !="missbath")
dfcoeff <- subset(dfcoeff,Variable !="missbed")
dfcoeff <- subset(dfcoeff,Variable !="misssqrft")
dfcoeff <- subset(dfcoeff,Variable !="n_bathroom")
dfcoeff <- subset(dfcoeff,Variable !="n_bedroom")
dfcoeff <- subset(dfcoeff,Variable !="live_sqrft")
dfcoeff <- subset(dfcoeff,Variable !="yr2014")
dfcoeff <- subset(dfcoeff,Variable !="yr2015")
dfcoeff <- subset(dfcoeff,Variable !="SS")
dfcoeff <- subset(dfcoeff,Variable !="FC1")
dfcoeff <- subset(dfcoeff,Variable !="FC2")
dfcoeff <- subset(dfcoeff,Variable !="REO1")
dfcoeff <- subset(dfcoeff,Value !=0)
write.csv(dfcoeff, "/work/coefs1315bigram.csv", row.names=TRUE)



dfcoeff2 <- dfcoeff[order(dfcoeff$Value),]
dfcoeff2[1:40,]
neglist <- dfcoeff2[1:20,]
neglist$weight <- neglist$Value * (-1)
wordcloud(neglist$Variable, neglist$weight,rot.per=0.4, random.order=FALSE, scale = c(3, 0.1),colors=brewer.pal(8, "Dark2") )

png("negative_wordcloud_1315_bigram.png", width=1280, height=800)
wordcloud(neglist$Variable, neglist$weight,rot.per=0.4, random.order=FALSE, scale = c(8, 0.2),colors=brewer.pal(8, "Dark2") )
dev.off()


dfcoeff2 <- dfcoeff[order(-dfcoeff$Value),]
dfcoeff2[1:40,]
dfcoeff2 <- subset(dfcoeff2,Variable !="(Intercept)")
dfcoeff2 <- subset(dfcoeff2,Variable !="missbath")
poslist <- dfcoeff2[1:20,]
poslist$weight <- poslist$Value * (1)

wordcloud(poslist$Variable, poslist$weight,rot.per=0.4, random.order=FALSE, scale = c(3, 0.1),colors=brewer.pal(8, "Dark2") )

png("positive_wordcloud_1315_bigram.png", width=1280, height=800)
wordcloud(poslist$Variable, poslist$weight,rot.per=0.4, random.order=FALSE, scale = c(8, 0.2),colors=brewer.pal(8, "Dark2") )
dev.off()












