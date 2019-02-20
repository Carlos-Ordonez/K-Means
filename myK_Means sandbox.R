rm(list = ls())

#create dataset
data(iris)
attach(iris)
irisCluster<- subset(iris, select = c("Petal.Width", "Sepal.Length"))
str(irisCluster)
#associate it with me

myData<-irisCluster
myData<-dougScore1
str(myData)
plot(myData)
#break apart into x and y components
#x<-myData[,1]
#y<-myData[,2]

#ncol because my data is a data frame
Components<-function(myData){
  components<-list()
  for (i in 1:ncol(myData)){
    components[[i]]<-myData[i]
  }
  return (components)
}

myDataComponents<-Components(myData)
str(myDataComponents)

#Creates index for the entire dataset and randomizes it
index <- 1:length(myData[[1]])
randomIndex<-sample(index,length(index),replace = FALSE)

##assign centroid index to first k randomized points
k <-2
int_Centroid_Index<-randomIndex[1:k]

#Create centroid
Centroids<-function(myDataComponents,k){
  Centroids<-list()
  for(i in 1:length(myDataComponents)){
    Centroids[[i]]<-myDataComponents[[i]][int_Centroid_Index[1:k],]
    #print(Centroids[i])
  }
  return (as.data.frame(Centroids))
  #return (Centroids)
}
centroids<-Centroids(myDataComponents,k)
str(centroids)

#Create data frame to hold each cluster
#cluster1<-data.frame(X = x[int_Centroid_Index[1]],Y = y[int_Centroid_Index[1]])
Clusters<-function(Centroids){
  clusters<-list()
  for(i in 1:k){
      clusters[[i]]<-Centroids[i,]
      print(i)
    }
  return(clusters)
}

clusters<-Clusters(centroids)
clusters
str(clusters)

#Calculates the euclidian distance between centroid and point
distance<-function(Centroids,Components,RI){
  distances<-list()
  sum = 0
  for (i in 1:ncol(Centroids)){
    for(j in 1:ncol(myData)){
      sum = sum + (Centroids[i,j]-Components[[j]][RI,])^2
      print(c(Centroids[i,j],Components[[j]][RI,]))
    } 
    distances[[i]]<- sqrt(sum)
    sum = 0
  }
  print(distances)
  return (distances)
}

for(i in 1:ncol(myData)){
  for(j in 1:length(randomIndex)){
    distances<-distance(centroids,myDataComponents,randomIndex[j])
    clusters[[which.min(distances)]][nrow(clusters[[which.min(distances)]][,i])+1,i]<-
      myDataComponents[[i]][randomIndex[j],]
  }
}

clusters
clusters[[which.min(distances)]][,2]
myDataComponents[[1]]

clusters[[which.min(distances)]][nrow(clusters[[which.min(distances)]])+1,1]<-
  myDataComponents[[1]][141,]
clusters[[which.min(distances)]][nrow(clusters[[which.min(distances)]]),2]<-
  myDataComponents[[2]][141,]



nrow(clusters[[which.min(distances)]])
clusters
plot(clusters[[1]])
str(myDataComponents)
clusters[[2]][1,]
myDataComponents[[1]][1,]
myDataComponents[[2]][1,]

#classifies points
for (i in k+1:(length(randomIndex)-k)){
  #creates initial dataframe to be added to cluster
  proposed_Point<- data.frame(X = x[i],Y = y[i])
  
  #calculate distance from proposed point to each centroid
  distance1<-distance(Centroid1,x,y,i)
  distance2<-distance(Centroid2,x,y,i)
  
  #puts proposed point in cluster with closest centroid
  if (distance1 < distance2){
    cluster1<- rbind(cluster1,proposed_Point)
  }
  if(distance2 < distance1){
    cluster2 <- rbind(cluster2,proposed_Point)
  }
} 

#Add id row to each cluster
cluster1$cID = rep(1,nrow(cluster1))
cluster2$cID = rep(2,nrow(cluster2))

#combine the two clusters in to one dataset for ploting
finalCluster<-rbind(cluster1,cluster2)

#plot both clusters with each centroid
plot(finalCluster$X,finalCluster$Y,col=ifelse(finalCluster$cID==1, "red", "blue"), cex = 1, pch = 19)
points(Centroid1, col = 'red', pch = 10, cex = 3)
points(Centroid2, col = 'blue', pch = 10, cex = 3)
