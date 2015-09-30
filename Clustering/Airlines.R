#Finding Similar customers belonging to an airline's frequent flyer program
setwd("/Users/akshaykulkarni/Documents/Analytics-Edge-Summer-2015/Clustering")

library(caret)
airlines = read.csv("../data/AirlinesCluster.csv")
summary(airlines)

#Pre-processing the data before Clustering. If we don't perform preProcessing i.e scaling and 
#normalizing of the data, clustering will be dominated by variables on larger scale

preProc = preProcess(airlines)
airlinesNorm = predict(preProc,airlines)
summary(airlinesNorm)

#Now running hierarchical clustering algorithm
distances = dist(airlinesNorm,method="euclidean") #Usinf euclidean distance as metric of similarity
clusters = hclust(distances,method="ward.D")
plot(clusters)
cluster_numbers = cutree(clusters,k=5) #Splitting the data into 5 clusters
HierCluster = split(airlinesNorm,cluster_numbers) #Subsetting the data into clusters

#Computing mean for each cluster for Balance value
tapply(airlinesNorm$Balance,cluster_numbers,mean)
#Comparing mean values with original data
tapply(airlines$Balance,cluster_numbers,mean)

#Alternatively use following approach to get centroid mean grouped by clusters
rm(split)
lapply(split(airlinesNorm,cluster_numbers),colMeans)

#Running KMeans on same data
set.seed(88)
KMeansClust = kmeans(airlinesNorm,centers=5,iter.max=1000)
table(KMeansClust$cluster)