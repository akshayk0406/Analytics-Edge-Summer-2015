library(caTools)
library(ROCR)
#Script to level of health care one is receving
#Splitted the data into training and test set
#Analysing result with different threshold value of cut off probabilty with the help of ROC Curve


setwd("/Users/akshaykulkarni/RProjects/AnalyticsEdge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/quality.csv",destfile = "quality.csv",method="curl")
quality <- read.csv('quality.csv')
str(quality)
table(quality$PoorCare)


set.seed(88)
split = sample.split(quality$PoorCare,SplitRatio = 0.75)

quality_train <- subset(quality , split==TRUE)
quality_test <- subset(quality , split==FALSE)

qualitylog <- glm(PoorCare ~ OfficeVisits + Narcotics , data = quality_train,family="binomial")
summary(qualitylog)
predict_train <- predict(qualitylog,type="response")
summary(predict_train)
tapply(predict_train,quality_train$PoorCare,mean)

table(quality_train$PoorCare,predict_train > 0.5)
table(quality_train$PoorCare,predict_train > 0.7)
table(quality_train$PoorCare,predict_train > 0.3)

ROCR_pred <- prediction(predict_train,quality_train$PoorCare)
ROCR_perf <- performance(ROCR_pred,"tpr","fpr")
plot(ROCR_perf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

predict_test <- predict(qualitylog,type="response",newdata=quality_test)
table(quality_test$PoorCare,predict_test > 0.3)

