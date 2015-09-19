#Movie Recommendation Engine
setwd("/Users/akshaykulkarni/RProjects/analyticsedge")
movies = read.table("data/u.item.txt",header=FALSE,sep="|",quote="\"")
colnames(movies) = c("Id","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure","Animation","Children","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror",'Musical',"Mystery","Romance","SciFi","Thriller","War","Western")

#Removing unnecessary columns
movies$Id = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

movies = unique(movies) #Removing Duplicates

#Implementing Hieracrhical Clustering
distances = dist(movies[2:20],method="euclidean")
clusterMovies = hclust(distances,method="ward.D")
plot(clusterMovies)

clusterGroups = cutree(clusterMovies,k=10) #Labeling data-points
tapply(movies$Action,clusterGroups,mean) #Avergae value of Action for each cluster
tapply(movies$Romance,clusterGroups,mean)

#Examples
subset(movies,Title=="Men in Black (1997)")
clusterGroups[257] #Men in Black went to cluster 2 which is for action adventure genre
cluster2 = subset(movies,clusterGroups==2) #Reteriving movies that belong to cluster 2