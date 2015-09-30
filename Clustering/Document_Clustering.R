#Analytic Edge HomeWork :- Document Clustering with Daily Kos
setwd("/Users/akshaykulkarni/Documents/Analytics-Edge-Summer-2015/Clustering")
dailykos = read.csv("../data/dailykos.csv")

#Using hierarchical clustering
distances = dist(dailykos[,-1],method="euclidean") #Computing distances between each pair of points
clusters = hclust(distances,method="ward.D") #Using ward.D method for hierarchical clustering
plot(clusters) #2 or 3 seems to be ideal cluster size
cluster_number = cutree(clusters,k=7) #Dividing data-points into 7 clusters


#Splitting data into 7 clusters
gp1 = subset(dailykos,cluster_number==1)
gp2 = subset(dailykos,cluster_number==2)
gp3 = subset(dailykos,cluster_number==3)
gp4 = subset(dailykos,cluster_number==4)
gp5 = subset(dailykos,cluster_number==5)
gp6 = subset(dailykos,cluster_number==6)
gp7 = subset(dailykos,cluster_number==7)
table(cluster_number) #To see distribution of items in clsuters

#Another way of doing above process
HierCluster = split(dailykos,cluster_number)

#Outputting columns(words) with high frequency in all cluster group
tail(sort(colMeans(gp1[-1])))
tail(sort(colMeans(gp2[-1])))

#Now doing analysis using K-means clustering
set.seed(1000)
result=kmeans(dailykos[-1],centers=7) #Again splitting data into 7 clusters
KMeansCluster = split(dailykos,result$cluster)
table(result$cluster) #Distribution of items in clusters
tail(sort(colMeans(KMeansCluster[[1]])))#Outputting 6 frequenct word in each cluster

#Comparing Output of KMeans and Hierarchical Clustering
table(cluster_number,result$cluster)