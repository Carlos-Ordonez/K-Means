my_K_Means<-function(myData,k){
  
  #Create list to hold component dataframes
  components<-list()
  for (i in 1:ncol(myData)){
    components[[i]]<-myData[i]
  }
  myDataComponents<-components
  
  #Creates index for the entire dataset and randomizes it
  index <- 1:length(myData[[1]])
  randomIndex<-sample(index,length(index),replace = FALSE)
  
  ##assign centroid index to first k randomized points
  int_Centroid_Index<-randomIndex[1:k]
  
  #Create list of centroid and turn into dataframe
  Centroids<-list()
  for(i in 1:length(myDataComponents)){
    Centroids[[i]]<-myDataComponents[[i]][int_Centroid_Index[1:k],]
  }
  centroids=as.data.frame(Centroids)
  
  #create list of clusters dataframes
  clusters<-list()
    for(i in 1:k){
      clusters[[i]]<-centroids[i,]
    }
  
  #Calculate the distance from each point to the centroids
  #Returns a list of distances
  distance<-function(Centroids,Components,RI){
    distances<-list()
    sum = 0
    for (i in 1:nrow(Centroids)){
      for(j in 1:ncol(myData)){
        sum = sum + (Centroids[i,j]-Components[[j]][RI,])^2
      }
      distances[[i]]<- sqrt(sum)
      sum = 0
    }
    return (distances)
  }
  
  #Classify points in to respective clusters 
  #using the distance list to index which cluster to classify as
  for(j in (k+1):length(randomIndex)){
    distances<-distance(centroids,myDataComponents,randomIndex[j])
    selected_Point<-c(myDataComponents[[1]][randomIndex[j],])
    
    for (s in 2:length(myDataComponents)){
      selected_Point[s]<-myDataComponents[[s]][randomIndex[j],]
    }
    clusters[[which.min(distances)]]<-rbind(clusters[[which.min(distances)]],selected_Point)
  }
  for (i in 1:k){
    clusters[[i]]$CID<-i
  }
  return(as.data.frame(rbindlist(clusters)))
}

Final_Cluster<-my_K_Means(myData,2)

plot(Final_Cluster[,1]~Final_Cluster[,2], col=Final_Cluster$CID + 3, cex = 1, pch = 19)

Final_Cluster
str(Final_Cluster)