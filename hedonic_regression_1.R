require(irlba)
require(tm)
require(wordcloud)
require(stringr)
require(slam)
require(glmnet)
require(caret)
require(matrixStats)


##################### load data  ###########################

setwd ("/work/")

#load data for sales in LA county in 2015. 
#comment.char command avoids treating # as comment.  eg 123 main street #3
LAcounty <- read.table("TEST_LA_2015.txt",sep="\t", header=FALSE, fill=TRUE, comment.char="", quote="")
dim(LAcounty)
cols <- c("ParcelID","closedate","closeprice","n_bathroom","n_bedroom","live_sqrft","SS_id","REO_id","FC_id","remarks","yearbuilt")
colnames(LAcounty) <- cols

#replace punctuation characters with white space.  removePunctuation command seems to join words and not create whitespace
LAcounty$remarksb <-str_replace_all(LAcounty$remarks, '[[:punct:]]',' ')

#data frame containing comments. will be used to generate document term matrix.
comments <-subset(LAcounty, select="remarksb")

#create corpus for use with tm package commands
corp <-Corpus(DataframeSource(comments))

########## clean text data, using tm package ###################
inspect(corp[1:2])

#transform letters to lower case
corp <- tm_map(corp, content_transformer(tolower))

# create stopword list
# add extra extra stop words to standard english stopword list
myStopwords <- c(stopwords('english'), "room", "home", "family", "bed", "bedroom", "bath","bedrooms" )
# remove  stopwords from list here
myStopwords <- setdiff(myStopwords, c("big"))
#apply stopword list
corp <- tm_map(corp, removeWords, myStopwords)

# numeric data such as number bedrooms, bathrooms and square feet will be contained
#in separate variables, not from comment section
corp <- tm_map(corp, removeNumbers) 
#note: removing punctuation will convert 3.5BR to 35BR
corp<- tm_map(corp, removePunctuation)  
#corp <- tm_map(corp, stripWhitespace)

#### convert corpus to document term matrix ##################

DTM <-DocumentTermMatrix(corp)
#create matrix from DTM
termMat <- as.matrix(DTM)
#create data frame
termDF <- as.data.frame(termMat)

#examine dimensions of document term matrix
ordmDTM <-termMat[,order(colSums(termMat),decreasing=T)]
dim(ordmDTM)
# 64906 listings , 25052 unique terms

# this keeps the 3000 most common words
#tmpmat <-ordmDTM[,1:3000]
#this keeps words with at least 50 entries.
tmpmat <-termDF[,colSums(termDF) > 50]

dim(tmpmat)
tmpDF <- data.frame(tmpmat)

#add back in bedroom, bathroom and sq ft variables
morevars <-subset(LAcounty, select=c("n_bathroom","n_bedroom", "live_sqrft"))
morevars$rownumber = 1:nrow(morevars)
tmpDF$rownumber = 1:nrow(tmpDF)
xvars <-merge(morevars,tmpDF, by="rownumber")

#set missing values to 0 and create missing value dummy variables
xvars$missbath <-with(xvars, ifelse(is.na(xvars$n_bathroom),1,0))
xvars$missbed <-with(xvars, ifelse(is.na(xvars$n_bedroom),1,0))
xvars$misssqrft <-with(xvars, ifelse(is.na(xvars$live_sqrft),1,0))
xvars$n_bathroom <-with(xvars, ifelse(is.na(xvars$n_bathroom),0,xvars$n_bathroom))
xvars$n_bedroom <-with(xvars, ifelse(is.na(xvars$n_bedroom),0,xvars$n_bedroom))
xvars$live_sqrft <-with(xvars, ifelse(is.na(xvars$live_sqrft),0,xvars$live_sqrft))

#remove 'rownumber' column from regression dataset
xvarsz <-subset(xvars, select=-rownumber)

# use log price as dependent variable
t1 <-subset(LAcounty, select="closeprice")
t1$lnprice <- log(t1$closeprice)
yvar  <- subset(t1, select="lnprice")

Mxvar =as.matrix(xvarsz)
Myvar=as.matrix(yvar)

#estimate regression using glmnet and 10 folds cross validation
# alpha =1 is LASSO
cv <- cv.glmnet(Mxvar,Myvar,alpha=.99, nfold=10) 
#coef(cv, s = "lambda.min")

#output coefficients from regression with the best cross validation performance.
matcoeff <- as.matrix(coef(cv, s = "lambda.min"))
#head(matcoeff, n=10)
#str(matcoeff)
write.csv(matcoeff, "/work/coefs.csv", row.names=TRUE)


# generate word cloud from coefficients
dfcoeff <- data.frame(matcoeff)
dfcoeff <- cbind(Variables = rownames(dfcoeff), dfcoeff)

cols <- c("Variable","Value")
colnames(dfcoeff) <- cols

dfcoeff2 <- dfcoeff[order(dfcoeff$Value),]
#print 40 variables with most negative coefficients
dfcoeff2[1:40,]

#generate wordcloud for words with 20 most negative coefs. Convert coefs to positive values
#so that words with most negative value have the biggest size.
neglist <- dfcoeff2[1:20,]
neglist$weight <- neglist$Value * (-1)
wordcloud(neglist$Variable, neglist$weight,rot.per=0.4, random.order=FALSE, scale = c(3, 0.1),colors=brewer.pal(8, "Dark2") )

#save file
png("negative_wordcloud.png", width=1280, height=800)
wordcloud(neglist$Variable, neglist$weight,rot.per=0.4, random.order=FALSE, scale = c(8, 0.2),colors=brewer.pal(8, "Dark2") )
dev.off()


dfcoeff2 <- dfcoeff[order(-dfcoeff$Value),]
dfcoeff2[1:40,]

#only keep variables associated with words.  Remove intercep and missing bath dummy
dfcoeff2 <- subset(dfcoeff2,Variable !="(Intercept)")
dfcoeff2 <- subset(dfcoeff2,Variable !="missbath")
poslist <- dfcoeff2[1:20,]
poslist$weight <- poslist$Value * (1)

wordcloud(poslist$Variable, poslist$weight,rot.per=0.4, random.order=FALSE, scale = c(3, 0.1),colors=brewer.pal(8, "Dark2") )

png("positive_wordcloud.png", width=1280, height=800)
wordcloud(poslist$Variable, poslist$weight,rot.per=0.4, random.order=FALSE, scale = c(8, 0.2),colors=brewer.pal(8, "Dark2") )
dev.off()



##### OLD CODE SNIPPETS####################



##### word count variable can be used in regression
#  wordcount <- rowSums(as.matrix(DTM))
#  wcdf <- as.data.frame(wordcount)
#  write.table(wcdf, "/work/wordcount.txt", sep="\t", row.names=FALSE)


##### keep most common terms for regression analysis
#  mDTM <- as.matrix(DTM)
#  dfDMT <-as.data.frame(mDTM)

#new code below.  keep words with at least 50 instances, rather than top 10k words.
#ordmDTM <-mDTM[,order(colSums(mDTM),decreasing=T)]
#dim(ordmDTM)
# 59402 listings , 24820 unique terms
# keep 10k most common terms.  this can be adjusted
#tmpmat <-ordmDTM[,1:10000]
#dim(tmpmat)
#tmpDF <- data.frame(tmpmat)
#write.csv(tmpDF, "/work/top10kterms.csv", row.names=FALSE)

### SVD
# see http://www-stat.wharton.upenn.edu/~stine/mich/DM_10.pdf
# full DTM is too big to run SVD.
#keep columns where words appear at least 50 times in listings
#  tmpDF <-dfDMT[,colSums(dfDMT) > 50]
#  dim(tmpDF)
#  write.csv(tmpDF, "/work/topterms.csv", row.names=FALSE)
#  tmpmat <-as.matrix(tmpDF)

# http://illposed.net/irlba.pdf
# still had difficulty running SVD. try irlba package
#udv <- svd(tmpmat); names(tmpmat)
#  L <- irlba(tmpmat, nu=20, nv=20)
#  plot(L$d)
#  U <- as.data.frame(L$u)
#  dim(U)
#  dim(tmpmat)

#  write.csv(U, "/work/SVD20.csv", row.names=FALSE)

#  tst <-t(L$u) %*% tmpmat
#  dim(t(tst))
#  U2 <- as.data.frame(t(tst))
#  dim(U2)

#  write.csv(U2, "/work/SVD20_v2.csv", row.names=FALSE)


