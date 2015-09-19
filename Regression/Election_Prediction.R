library(mice)
#Script to predict winner of election in 2008 given the data for election 2004,2008
#Also deals with missing value
#Improvised Baseline model against Logistic Regression model is compared

setwd("/Users/akshaykulkarni/RProjects/AnalyticsEdge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/PollingData.csv",destfile="poll.csv",method="curl")
polling = read.csv("poll.csv")
table(polling$Year)
summary(polling) #To see missing data

simple = polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
summary(polling)
set.seed(144)
imputed = complete(mice(simple)) #Handling missing values

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

train = subset(polling,Year==2004 | Year == 2008)
test = subset(polling,Year == 2012)
table(train$Republican)
table(sign(train$Rasmussen))
table(train$Republican,sign(train$Rasmussen))

cor(train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])
#PropR is best option here
model1 = glm(Republican ~ PropR,data=train,family=binomial)
summary(model1)
predict_train = predict(model1,type="response")
table(train$Republican,predict_train>=0.5)

model2 = glm(Republican ~ SurveyUSA + DiffCount,data=train,family = binomial)
summary(model2)
predict_train_2 = predict(model2,type="response")
table(train$Republican,predict_train_2>=0.5)

table(test$Republican,sign(test$Rasmussen))
test_pred = predict(model2,type="response",newdata=test)
table(test$Republican,test_pred>=0.5)