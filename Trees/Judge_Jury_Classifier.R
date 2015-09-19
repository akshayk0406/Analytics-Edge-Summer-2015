library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)

#Using CART trees to analyse the decision of Justice stevens
setwd("/Users/akshaykulkarni/RProjects/analyticsedge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/stevens.csv",destfile="stevens.csv",method="curl")
stevens = read.csv("stevens.csv")
summary(stevens)
str(stevens)

#Splitting the data into traning and test set
set.seed(3000)
split = sample.split(stevens$Reverse,SplitRatio=0.7)
train_steven = subset(stevens,split==TRUE)
test_steven = subset(stevens,split==FALSE)

nrow(stevens)
nrow(test_steven)

#minbucket decides when to stop splitting
#Low value of minbucket may lead to avoid over-fitting
#High value of minbucket may lead to model with poor accuracy
tree_stevens = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                     data=train_steven,method="class",minbucket=25)

prp(tree_stevens) #Plotting the CART tree

#Predicting on test data
predict_cart = predict(tree_stevens,newdata=test_steven,type="class")
table(test_steven$Reverse,predict_cart)

#ROCR Curve to analysze the model
predictROC = predict(tree_stevens,newdata=test_steven)
pred = prediction(predictROC[,2],test_steven$Reverse)
perf = performance(pred,"tpr","fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

#Now Using RandomForest for prediction
set.seed(200)
train_steven$Reverse = as.factor(train_steven$Reverse)
test_steven$Reverse = as.factor(test_steven$Reverse)
steven_forest = randomForest(Reverse ~ Circuit + Issue + Petitioner 
                             + Respondent + LowerCourt + Unconst,
                             data=train_steven,nodesize=25,ntree=200)
predict_forest = predict(steven_forest,newdata=test_steven)
table(test_steven$Reverse,predict_forest)

library(caret)
library(e1071)
#how to select best minbucket size for CART tress.Using cross-validation to fix cp paramter
numFolds = trainControl(method="cv",number=10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
        data = train_steven,method="rpart",trControl=numFolds,tuneGrid=cpGrid)
steven_treeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                      data=train_steven,method="class",cp=0.19)
predictcv = predict(steven_treeCV,newdata=test_steven,type="class")
table(test_steven$Reverse,predictcv)
prp(steven_treeCV)


