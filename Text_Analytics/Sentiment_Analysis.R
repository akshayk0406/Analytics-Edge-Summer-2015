setwd("/Users/akshaykulkarni/RProjects/analyticsedge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/tweets.csv",destfile="data/tweet.csv",method="curl")
tweet = read.csv("data/tweet.csv",stringsAsFactors=FALSE)
str(tweet)

tweet$Negative = as.factor(tweet$Avg<=-1)
table(tweet$Negative)

#Pre-processing text-data before using bag of words model
library(tm)
library(SnowballC)

#Pre-processing on text-data. Removing stop words,stemming
corpus = Corpus(VectorSource(tweet$Tweet))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus,removeWords,c("apple",stopwords("english"))) #Removing word apple as it appears in all tweets
corpus = tm_map(corpus,stemDocument)

frequencies = DocumentTermMatrix(corpus) #Dimension of frequencies is Number of Docs * Number of words
#We see that frequencies matrix is sparse. So we try to reduce dimension of frequencies matrix
findFreqTerms(frequencies,lowfreq=20) #Finding terms which occur at least 20 times
sparse = removeSparseTerms(frequencies,0.995) #keep terms which appear in at least 0.5% of tweets
tweetSparse = as.data.frame(as.matrix(sparse))
colnames(tweetSparse) = make.names(colnames(tweetSparse)) #Making sure variables are correctly named accorind to conventions in R language
tweetSparse$Negative = tweet$Negative

library(caTools)
set.seed(123)
split = sample.split(tweetSparse$Negative,SplitRatio=0.70)
trainSparse = subset(tweetSparse,split==TRUE)
testSparse = subset(tweetSparse,split==FALSE)

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~.,data=trainSparse,method="class")
prp(tweetCART)
predictCART = predict(tweetCART,newdata=testSparse,type="class")
table(testSparse$Negative,predictCART)
table(testSparse$Negative) #BaseLine Model

library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative~.,data=trainSparse)
predictRF = predict(tweetRF,newdata=testSparse)
table(testSparse$Negative,predictRF)