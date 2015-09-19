setwd("/Users/akshaykulkarni/RProjects/analyticsedge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/healthy.csv",destfile="data/healthy.csv",method="curl")

healthy = read.csv("data/healthy.csv",header=FALSE)
healthyMatrix = as.matrix(healthy)
healthyVector = as.vector(healthyMatrix)
#distance = dist(healthyVector,method="euclidean") This is dangerous as healthyVector has 
#length of 65000 and finding pair-wise distance will need more memory hence we use k-means clustering

num_cluster = 5
set.seed(1)
KMC = kmeans(healthyVector,centers=num_cluster,iter.max=1000)
healthyClusters = KMC$cluster
KMC$centers #Gives mean intensity value

#Now outputting segmented image
dim(healthyClusters) = c(nrow(healthyMatrix),ncol(healthyMatrix))
image(healthyClusters,axes=FALSE,col=rainbow(num_cluster))

#Now detecting if given image represent whether person has tumor or not
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/tumor.csv",destfile="data/tumor.csv",method="curl")
tumor = read.csv("data/tumor.csv",header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

#Treat healthy vector as traning set and tumorVector as test set
library(flexclust)
KMC.kcca = as.kcca(KMC,healthyVector)
tumorClusters = predict(KMC.kcca,tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix),ncol(tumorMatrix))
image(tumorClusters,axes=FALSE,col=rainbow(num_cluster)) #Image shows presence of tumor