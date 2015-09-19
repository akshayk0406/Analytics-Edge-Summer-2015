setwd("/Users/akshaykulkarni/RProjects/analyticsedge")
claims = read.csv("ClaimsData.csv")
str(claims)
table(claims$bucket2009)/nrow(claims)

library(caTools)
set.seed(88)
split = sample.split(claims$bucket2009,SplitRatio=0.6)
train = subset(claims,split==TRUE)
test = subset(claims,split==FALSE)

#BaseLine method
#According to D2Hawkeye,Cost in 2009 will be same as 2008
table(test$bucket2009,test$bucket2008) #Accuracy comes out be 68.8
penalty_matrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow=TRUE,nrow=5)
as.matrix(table(test$bucket2009,test$bucket2008))*penalty_matrix #Total Penalty
sum(as.matrix(table(test$bucket2009,test$bucket2008))*penalty_matrix)/nrow(test) #Penalty error

library(rpart)
library(rpart.plot)

claims_tree = rpart(bucket2009 ~ age+arthritis+alzheimers+cancer+copd+depression+diabetes
                    +heart.failure+ihd+kidney+osteoporosis+stroke+bucket2008+reimbursement2008,
                    data=train,method="class",cp=0.00005)
prp(claims_tree)

predict_test = predict(claims_tree,newdata=test,type="class")
table(test$bucket2009,predict_test) #Accuracy comes out to be 0.713
sum(as.matrix(table(test$bucket2009,predict_test))*penalty_matrix)/nrow(test) #Comes out to be 0.757

#We have specified a penalty matrix while building the model.If rpart knows we are imposing bigger
#penalties for some kind of error it will choose its split accordingly to minimize the error

claims_tree = rpart(bucket2009 ~ age+arthritis+alzheimers+cancer+copd+depression+diabetes
                    +heart.failure+ihd+kidney+osteoporosis+stroke+bucket2008+reimbursement2008,
                    data=train,method="class",cp=0.00005,parms=list(loss=penalty_matrix))

predict_test = predict(claims_tree,newdata=test,type="class")
table(test$bucket2009,predict_test) #Accuracy comes out to be 0.647
sum(as.matrix(table(test$bucket2009,predict_test))*penalty_matrix)/nrow(test) #Comes out to be 0.641
#We observe penalty error is reduced but accuracy is also reduced