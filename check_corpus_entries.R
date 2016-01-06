# code was edited from other code.  all packages might not be required for this analysis.
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

#comment.char command avoids treating # as comment.  eg 123 main street #3
LAcounty <- read.table("TEST_LA_2015.txt",sep="\t", header=FALSE, fill=TRUE, comment.char="", quote="")
dim(LAcounty)
cols <- c("ParcelID","closedate","closeprice","n_bathroom","n_bedroom","live_sqrft","SS_id","REO_id","FC_id","remarks","yearbuilt")
colnames(LAcounty) <- cols

#replace punctuation characters with white space.  removePunctuation command seems to join words and not create whitespace
LAcounty$remarksb <-str_replace_all(LAcounty$remarks, '[[:punct:]]',' ')

# create dataset of remarks
comments <-subset(LAcounty, select="remarksb")

#create corpus for use with tm package commands
corp <-Corpus(DataframeSource(comments))

#convert words to lower case
corp <- tm_map(corp, content_transformer(tolower))

# create stopword list
# add extra extra stop words to standard english stopword list
myStopwords <- c(stopwords('english'), "room", "home", "family", "bed", "bedroom", "bath","bedrooms" )
# remove  stopwords from list here
myStopwords <- setdiff(myStopwords, c("big"))
#remove stopwords
corp <- tm_map(corp, removeWords, myStopwords)
#remove numbers.  Other variables in the regression will contain number of bedrooms
#number of bathrooms and square feet
corp <- tm_map(corp, removeNumbers) # derive numeric variables from other fields in dataset
corp<- tm_map(corp, removePunctuation)  # will convert 3.5ba to 35ba
#corp <- tm_map(corp, stripWhitespace)

#create document term matrix
DTM <-DocumentTermMatrix(corp)
#convert to matrix
termMat <- as.matrix(DTM)
#convert to data frame
termDF <- as.data.frame(termMat)

#might need the row numbers in order to identify a specific row
termDF2 <- cbind(Variables = rownames(termDF), termDF)

#this command creates dataset containing the word 'montana'
termDF3 <-subset(termDF2, montana > 0)
# based on dataframe 'termDF3', row 1615 of the corpus contained the word montana
# the inspect command will print out the full realtor comments for this
# entry containing the word montana, so that the context can be reviewed
inspect(corp[1615])

termDF3 <-subset(termDF2, medio > 0)
inspect(corp[1210])

termDF3 <-subset(termDF2, swamp > 0)
inspect(corp[421])
inspect(corp[2019])



