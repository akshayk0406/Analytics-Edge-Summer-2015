setwd("/Users/akshaykulkarni/RProjects/analyticsedge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/WHO.csv",destfile="data/WHO.csv",method="curl")

who = read.csv("data/WHO.csv")
plot(who$FertilityRate ~ who$GNI)
library(ggplot2)

scplot = ggplot(who,aes(x=GNI,y=FertilityRate))
fertility_gni_plot = scplot + geom_point(color="darkred",size=3,shape=8) + ggtitle("Fertility Rate vs Gross National Income")
pdf("data/fertility_gni_plot.pdf")
print(fertility_gni_plot)
dev.off()

ggplot(who,aes(x=GNI,y=FertilityRate,color=Region)) + geom_point()
ggplot(who,aes(x=GNI,y=FertilityRate,color=LifeExpectancy)) + geom_point() #Coloring by numerical color hence we get color gradient
ggplot(who,aes(x=FertilityRate,y=Under15)) + geom_point()
ggplot(who,aes(x=log(FertilityRate),y=Under15)) + geom_point()

#Bulidng Linear reg model
model = lm(Under15 ~ log(FertilityRate),data=who)
summary(model)
#Visualization made us realize that log transformation makes better model

#Regression line to plot
ggplot(who,aes(x=log(FertilityRate),y=Under15)) + geom_point() + stat_smooth(method="lm",level=0.99) #99% Confidence Interval
ggplot(who,aes(x=log(FertilityRate),y=Under15)) + geom_point() + stat_smooth(method="lm",se=FALSE) 

#Predictive Policing in Chicago Area
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/mvt.csv",destfile="data/mvt.csv",method="curl")
mvt = read.csv("data/mvt.csv",stringsAsFactors=FALSE)
mvt$Date = strptime(mvt$Date,format="%m/%d/%y %H:%M")
mvt$WeekDay = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
week_day_crime = as.data.frame(table(mvt$WeekDay))

#Line-Plot
ggplot(week_day_crime,aes(x=Var1,y=Freq)) + geom_line(aes(group=1))
week_day_crime$Var1 = factor(week_day_crime$Var1,ordered=TRUE,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
ggplot(week_day_crime,aes(x=Var1,y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the week") + ylab("Total Motor Vechicle Theft")

#HeatMaps
DayHourCounts = as.data.frame(table(mvt$WeekDay,mvt$Hour))
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
ggplot(DayHourCounts,aes(x=Hour,y=Freq))+geom_line(aes(group=Var1,color=Var1),size=2)
#Each line correspond to day of the week

DayHourCounts$Var1 = factor(DayHourCounts$Var1,ordered=TRUE,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")) #Ordering Factor variable
ggplot(DayHourCounts,aes(x=Hour,y=Var1)) + geom_tile(aes(fill=Freq))  + scale_fill_gradient(name="Total MV Thefts",low="white",high="red")  + theme(axis.title.y=element_blank()) #For heat-map sue geom_tile

library(ggmap)
library(maps)

chicago = get_map(location="chicage",zoom=11)
ggmap(chicago) + geom_point(data=mvt[1:100,],aes(x=Longitude,y=Latitude))
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))
LatLonCounts$Lon = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))
ggmap(chicago) + geom_point(data=LatLonCounts,aes(x=Lon,y=Lat,color=Freq,size=Freq)) + scale_color_gradient(low="yellow",high="red")
