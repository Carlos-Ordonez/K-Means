rm(list = ls())

#create dataset
data(iris)
attach(iris)
irisCluster<- subset(iris, select = c("Petal.Width", "Sepal.Length"))

#associate it with me
myData<-irisCluster


plot(myData)
#break apart into x and y components
x<-myData[,1]
y<-myData[,2]

#Creates index for the entire dataset and randomizes it
index <- 1:nrow(myData)
randomIndex<-sample(index,length(index),replace = FALSE)

##assign centroid index to first k randomized points
k <-2
int_Centroid_Index<-randomIndex[1:k]

#Create centroid
Centroid1<-myData[int_Centroid_Index[1],]
Centroid2<-myData[int_Centroid_Index[2],]

#Create data frame to hold each cluster
cluster1<-data.frame(X = x[int_Centroid_Index[1]],Y = y[int_Centroid_Index[1]])
cluster2<-data.frame(X = x[int_Centroid_Index[2]],Y = y[int_Centroid_Index[2]])

#Calculates the euclidian distance between centroid and point
distance<-function(C,x,y,i){
  distance <- sqrt((C[,1]-x[i])^2+(C[,2]-y[i])^2)
  return(distance)
}
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
