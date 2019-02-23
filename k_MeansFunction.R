my_K_Means<-function(myData,k){
  
  pause_time<-.5
  start_time<-Sys.time()
  
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
  new_centroids<-centroids
  current_centroids<-data.frame()
  itterations<-0
  while(isTRUE(all.equal(new_centroids,current_centroids))==FALSE){
    itterations<-itterations +1
    centroids<-new_centroids
    clusters<-list()
    for(i in 1:k){
      clusters[[i]]<-new_centroids[i,]
    }
    for(j in (k+1):length(randomIndex)){
      distances<-distance(centroids,myDataComponents,randomIndex[j])
      
      selected_Point<-c(myDataComponents[[1]][randomIndex[j],])
      for (s in 2:length(myDataComponents)){
        selected_Point[s]<-myDataComponents[[s]][randomIndex[j],]
      }
      
      clusters[[which.min(distances)]]<-rbind(clusters[[which.min(distances)]],selected_Point)
    }
    current_centroids<-centroids
    for (i in 1:nrow(centroids)){
      for (j in 1:length(components)){
        new_centroids[i,j]<-sum(clusters[[i]][,j]/length(clusters[[i]][,j]))
      }
    }
    print(new_centroids)
    for (i in 1:k){
      clusters[[i]]$CID<-i
    }
    cluster_Itteration<-as.data.frame(rbindlist(clusters))
    plot(cluster_Itteration[,1]~cluster_Itteration[,2], col=cluster_Itteration$CID + 3, cex = 1.5, pch = 20)
    
    for (i in 1:nrow(centroids)){
      points(centroids[i,2],centroids[i,1],col = "red", cex = 3,pch = 10)
    }
    
    Sys.sleep(pause_time)
    
  }
  print(paste("Itterations:", itterations))
  end_time<-Sys.time()
  print((end_time-start_time)-(itterations*pause_time))
  
  return(as.data.frame(rbindlist(clusters)))
}

Final_Cluster<-my_K_Means(myData,3)


Centroids
Final_Cluster
str(Final_Cluster)









