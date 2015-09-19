#Simple Script predict number of wins BaseBall team will have in season 2002
setwd("/Users/akshaykulkarni/RProjects/AnalyticsEdge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/baseball.csv",destfile="baseball.csv",method="curl")
baseball = read.csv("baseball.csv")

moneyball = baseball[baseball$Year<2002,]
moneyball$RD = moneyball$RS-moneyball$RA
plot(moneyball$RD,moneyball$W)

WinsReg = lm(W ~ RD,data=moneyball)
summary(WinsReg)

#BA is overvalued by scouts
RunsReg = lm(RS ~ OBP + SLG + BA , data = moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG , data = moneyball)
summary(RunsReg)