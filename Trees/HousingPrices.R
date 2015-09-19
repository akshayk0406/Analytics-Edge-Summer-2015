#Investigate how air-pollution affects prices of land/houses
setwd("/Users/akshaykulkarni/RProjects/analyticsedge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/boston.csv",destfile="boston.csv",method="curl")
boston = read.csv('boston.csv')
str(boston)

#Getting idea about the data
plot(boston$LON,boston$LAT)
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col="blue",pch=19) #plotting the points which are on CHAS river
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red",pch=19) #plotting MIT on graph
summary(boston$NOX)
points(boston$LON[boston$NOX>=0.55],boston$LAT[boston$NOX>=0.55],col="green",pch=19)

#Distribution of housing prices
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)

plot(boston$LAT,boston$MEDV)
plot(boston$LON,boston$MEDV)

latlonlm = lm(MEDV ~ LAT + LON , data=boston)
summary(latlonlm)

plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)
points(boston$LON[latlonlm$fitted.values>=21.2],boston$LAT[latlonlm$fitted.values>=21.2],col="blue",pch="$") #linear regression not doing good job

library(rpart)
library(rpart.plot)

latlontree = rpart(MEDV ~ LAT + LON,data=boston)
prp(latlontree)

plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)
fittedvalues = predict(latlontree)
plot(boston$LON,boston$LAT)
points(boston$LON[latlonlm$fitted.values>=21.2],boston$LAT[latlonlm$fitted.values>=21.2],col="blue",pch="$")

latlontree = rpart(MEDV ~ LAT + LON,data=boston,minbucket=50) #To avoid over-fitting,minbucket parameter was added
plot(latlontree)
text(latlontree)

#Focusing on lowest-price
plot(boston$LON,boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17) #Correctly predicts south-boston area as the area with low house prices

points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)

library(caTools)
set.seed(123)
split = sample.split(boston$MEDV,SplitRatio=0.7)
train = subset(boston,split==TRUE)
test = subset(boston,split==FALSE)

#First using linear-regression
linreg = lm(MEDV ~ LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data=train)
summary(linreg)
linreg.pred = predict(linreg,newdata=test)
linreg.sse = sum((linreg.pred-test$MEDV)^2)

#Now using CART trees
tree = rpart(MEDV ~ LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data=train)
prp(tree)
tree.pred = predict(tree,newdata=test)
tree.sse = sum((tree.pred-test$MEDV)^2)

#We see that CART does not perform better in this case as squared-error is much more

#Now using cross-validation to improve performance
library(caret)
library(e1071)

tr.control = trainControl(method="cv",number=10)
cp.grid = expand.grid(.cp=(0:10)*0.001) #Range of cp values to try and pick best one
tr = train(MEDV~LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data=train,
           method="rpart",trControl=tr.control,tuneGrid=cp.grid)

best.tree = tr$finalModel
prp(best.tree)

best_tree_predict = predict(best.tree,newdata=test)
best_tree_sse = sum((best_tree_predict-test$MEDV)^2)

#Cross-validation improved accuracy but still linear regression out-performed CART tree models

