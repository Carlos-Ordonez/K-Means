rm(list = ls())

#create dataset
dougScore <- read_csv("C:/Users/Carlo/OneDrive/Documents/DougScore  - Sheet1.csv")
colnames(dougScore)<- c("Model Year",
                        "Make",
                        "Model",
                        "W_Styling",
                        "W_Acceleration",
                        "W_Handeling",
                        "W_FunFactor",
                        "W_CoolFactor",
                        "W_Total",
                        "D_Features", 
                        "D_Comfort",
                        "D_Quality",
                        "D_Practicality",
                        "D_Value",
                        "D_Total",
                        "DougScore",
                        "VideoLink",
                        "City",
                        "State",
                        "Vehicle.Origin" )

dougScore1<-as.data.frame(dougScore[-c(1,2),])
str(dougScore1)
#associate it with me
myData<-dougScore1[c(9,15,16)]
colnames(myData)<-c("X","Y","Z")
myData
plot(myData)
#break apart into x and y components
x<-myData[,1]
y<-myData[,2]
z<-myData[,3]
plot(y~x~z)
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
cluster1<-data.frame(X = x[int_Centroid_Index[1]],Y = y[int_Centroid_Index[1]],Z = z[int_Centroid_Index[1]])
cluster2<-data.frame(X = x[int_Centroid_Index[2]],Y = y[int_Centroid_Index[2]],Z = z[int_Centroid_Index[2]])

#Calculates the euclidian distance between centroid and point
distance<-function(C,x,y,z,i){
  distance <- sqrt((C[,1]-x[i])^2+(C[,2]-y[i])^2+ (C[,3]-z[i])^2)
  return(distance)
}
#classifies points
for (i in k+1:(length(randomIndex)-k)){
  #creates initial dataframe to be added to cluster
  #i =1
  proposed_Point<- data.frame(X = x[i], Y = y[i], Z = z[i])
  
  #calculate distance from proposed point to each centroid
  distance1<-distance(Centroid1,x,y,z,i)
  distance2<-distance(Centroid2,x,y,z,i)
  
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
cluster3$cID = rep(3,nrow(cluster3))

#combine the two clusters in to one dataset for ploting
finalCluster<-rbind(cluster1,cluster2,cluster3)

#plot both clusters with each centroid
plot(finalCluster$X,finalCluster$Y,col=ifelse(finalCluster$cID==1, "red", "blue"), cex = 1, pch = 19)
points(Centroid1, col = 'red', pch = 10, cex = 4)
points(Centroid2, col = 'blue', pch = 10, cex = 4)

#points(N_Centroid1, col = 'red', pch = 10, cex = 4)
#points(N_Centroid2, col = 'blue', pch = 10, cex = 4)


# N_Centroid1<-data.frame(X = (sum(cluster1[1])/nrow(cluster1)), 
#                         Y = (sum(cluster1[2])/nrow(cluster1)))
# N_Centroid2<-data.frame(X =(sum(cluster2[1])/nrow(cluster2)),
#                         Y =(sum(cluster2[2])/nrow(cluster2)))
# 
# 
# Centroid1<-N_Centroid1
# Centroid2<-N_Centroid2
