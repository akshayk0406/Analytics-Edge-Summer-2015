#Script to Analysze emails from top-level officials at ENRON and detection whether they were involved in scam/corruption
setwd("/Users/akshaykulkarni/RProjects/analyticsedge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/energy_bids.csv",destfile="data/energy_bids.csv",method="curl")
emails = read.csv("data/energy_bids.csv",stringsAsFactors=FALSE)
str(emails)
table(emails$responsive)

#Pre-process the corpus
library(tm)
corpus = Corpus(VectorSource(emails$email))
corpus = tm_map(corpus,tolower) #Converting to lowercase
corpus = tm_map(corpus,PlainTextDocument)
corpus = tm_map(corpus,removePunctuation) #Removing Punctuations
corpus = tm_map(corpus,removeWords,stopwords("english")) #Remove StopWords
corpus = tm_map(corpus,stemDocument) #Stemming of words in corpus

#dtm -> Document term matrix
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm,0.97) #Removing the term that don't occur in at least 3 % of document
labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive

#Splitting data into training and testing set
library(caTools)
set.seed(144)
split = sample.split(labeledTerms$responsive,SplitRatio = 0.7)
train = subset(labeledTerms,split==TRUE)
test = subset(labeledTerms,split==FALSE)

#Using CART for building model and making predictions
library(rpart)
library(rpart.plot)

emailCART = rpart(responsive ~.,data=train,method="class")
prp(emailCART)
predictCART = predict(emailCART,newdata=test)
table(test$responsive,predictCART[,2]>=0.5) #Using 0.5 as threshold

#False Negative must be avoided
library(ROCR)
predROCR = prediction(predictCART[,2],test$responsive)
perf = performance(predROCR,"tpr","fpr")
plot(perf,colorize=TRUE)
performance(predROCR,"auc")@y.values
