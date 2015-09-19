setwd("/Users/akshaykulkarni/RProjects/AnalyticsEdge")
library(RCurl)
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/wine.csv",destfile="wine.csv",method="curl")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/wine_test.csv",destfile="wines_test.csv",method="curl")
wine=read.csv('wine.csv')
wine_test=read.csv('wines.csv')

model1 = lm(Price ~ AGST,data=wine)
summary(model1)
model1$residuals
SSE = sum(model1$residuals^2)

model2 = lm(Price ~ AGST+HarvestRain,data=wine)
summary(model2)
SSE = sum(model2$residuals^2)

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop,data=wine)
summary(model3)
sum(model3$residuals^2)

cor(wine$Age , wine$FrancePop)

model4 = lm(Price ~ HarvestRain + WinterRain,data=wine)
summary(model4)

model5 = lm(Price ~ AGST + HarvestRain + WinterRain + Age , data=wine)
summary(model5)

result = predict(model5,newdata=wine_test)
sse = sum((wine_test$Price-result)^2)
sst = sum((wine_test$Price-mean(wine$Price))^2)
1-(sse/sst)