library(caTools)
library(ROCR)
#Script to predict whether person has chance of CHD 10 years down the line
#Plotting ROC curve to better understand our models
setwd("/Users/akshaykulkarni/RProjects/AnalyticsEdge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/framingham.csv",
              destfile="heart.csv",
              method="curl")

heart = read.csv("heart.csv")
set.seed(1000)
split = sample.split(heart,SplitRatio=0.65)
htrain = subset(heart,split == TRUE)
htest = subset(heart,split == FALSE)

hmodel = glm(TenYearCHD ~ .,data=htrain,family=binomial)
summary(hmodel)

predict_test = predict(hmodel,type="response",newdata=htest)
table(htest$TenYearCHD,predict_test>0.5)

ROCR_pred = prediction(predict_test,htest$TenYearCHD)
as.numeric(performance(ROCR_pred,"auc")@y.values)