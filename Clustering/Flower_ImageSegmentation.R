setwd("/Users/akshaykulkarni/RProjects/analyticsedge")
download.file("https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/flower.csv",destfile="data/flower.csv",method="curl")

flower = read.csv("data/flower.csv",header=FALSE) #Pixel Intensity Matrix
flowerMatrix = as.matrix(flower) #This is crucial step. First convert input data-frame to matrix. Then convert the resulting matrix to vector
flowerVector = as.vector(flowerMatrix)
distance = dist(flowerVector,method="euclidean")

clusterIntensity = hclust(distance,method="ward.D") #Minimize variance within each cluster and distance among cluster
plot(clusterIntensity)
rect.hclust(clusterIntensity,k=3,border="red")
flowerClusters = cutree(clusterIntensity,k=3)
tapply(flowerVector,flowerClusters,mean)
dim(flowerClusters) = c(50,50)
image(flowerClusters,axes=FALSE)
image(flowerMatrix,axes=FALSE,col = grey(seq(0,1,length=256)))
