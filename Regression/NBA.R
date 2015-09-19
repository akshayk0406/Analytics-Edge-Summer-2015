#Script to predict whether NBA will make to playoffs or not
#Analysing contribution of different independent variables to the model with the help summary function in R
#Analysing result with baseline model

setwd("/Users/akshaykulkarni/RProjects/AnalyticsEdge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/NBA_train.csv",destfile="nba_train.csv",method="curl")
nba_train = read.csv('nba_train.csv')

download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/NBA_test.csv",destfile="nba_test.csv",method="curl")
nba_test = read.csv('nba_test.csv')

table(nba_train$W,nba_train$Playoffs)
nba_train$PD = nba_train$PTS-nba_train$oppPTS
plot(nba_train$PD,nba_train$W)

winsReg = lm(W ~ PD,data=nba_train)
summary(winsReg)

PointsReg = lm(PTS ~ X2PA + X3PA + FTA + ORB + DRB + TOV + AST + STL + BLK,data=nba_train)
summary(PointsReg)
SSE = sum((PointsReg$residuals^2))
RMSE = sqrt(SSE/nrow(nba_train))

mean(nba_train$PTS)

PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + ORB + DRB +  AST + STL + BLK,data=nba_train)
summary(PointsReg2)

PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + ORB +  AST + STL + BLK,data=nba_train)
summary(PointsReg3)

PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + ORB + DRB +  AST + STL ,data=nba_train)
summary(PointsReg4)

SSE4 = sum((PointsReg4$residual)^2)
RMSE = sqrt(SSE4/nrow(nba_train))

pts_prediction = predict(PointsReg4,newdata=nba_test)
sse_test = sum((pts_prediction-nba_test$PTS)^2)
sst_test = sum((mean(nba_train$PTS) - nba_test$PTS)^2)
r2 = 1 - (sse_test/sst_test)