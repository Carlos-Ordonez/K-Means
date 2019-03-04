library(data.table)
my_K_Means<-function(myData,k){
  
  pause_time<-0
  start_time<-Sys.time()
  
  # Create a list of dataframes for each component of myData
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
  
  #Create list of centroids and turn it into a dataframe
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
  
  # creates list of distances to compute universal distance
  Cluster_Distance<-list()
  for (i in 1:k){
    Cluster_Distance[[i]]<-data.frame(Distance = 0)
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
    
    #clear list of distances
    Cluster_Distance<-list()
    for (i in 1:k){
      Cluster_Distance[[i]]<-data.frame(Distance = 0)
    }
    
    for(i in 1:k){
      clusters[[i]]<-new_centroids[i,]
    }
    for(j in (k+1):length(randomIndex)){
      distances<-distance(centroids,myDataComponents,randomIndex[j])
      
      # Starts construction of selected point
      selected_Point<-c(myDataComponents[[1]][randomIndex[j],])
      # Builds selected point
      for (s in 2:length(myDataComponents)){
        selected_Point[s]<-myDataComponents[[s]][randomIndex[j],]
      }
      # Builds list of smallest distances
      Cluster_Distance[[which.min(distances)]][nrow(Cluster_Distance[[which.min(distances)]])+1,]<-distances[[which.min(distances)]]
      # Builds Clusters
      clusters[[which.min(distances)]]<-rbind(clusters[[which.min(distances)]],selected_Point)
    }
    # Creates New Centroids while keeping track of old centroid for comparison
    current_centroids<-centroids
    for (i in 1:nrow(centroids)){
      for (j in 1:length(components)){
        new_centroids[i,j]<-sum(clusters[[i]][,j]/length(clusters[[i]][,j]))
      }
    }
    
    # Adds cluster classification to new clusters 
    for (i in 1:k){
      clusters[[i]]$CID<-i
    }
    # combines all clusters into single dataframe
    cluster_Itteration<-as.data.frame(rbindlist(clusters))
    
    # plots current clusters
    plot(cluster_Itteration[,1]~cluster_Itteration[,2],
         col=cluster_Itteration$CID + 3,
         cex = 1.5,
         pch = cluster_Itteration$CID + 5)
    
    # plots current centroid ontop of clusters
    for (i in 1:nrow(centroids)){
      points(centroids[i,2],
             centroids[i,1],
             col = "red",
             cex = 3,
             pch = 10)
    }
    # pause for visual purposes
    Sys.sleep(pause_time)
  }
  #---------------------------ALGORITHM DONE--------------------------------------#
  
  # Calcultes Average distance to centroids
  C_Distances<-Cluster_Distance
  avg_dis<-0
  for (i in 1:length(C_Distances)){
    avg_dis<-(avg_dis + (sum(C_Distances[[i]])/nrow(C_Distances[[i]])))
  }
  avg_Cluster_Distance<<-avg_dis/length(C_Distances)
  
  print(paste("Global Distance:", avg_Cluster_Distance))
  print(paste("Itterations:", itterations))
  end_time<-Sys.time()
  print((end_time-start_time)-(itterations*pause_time))
  return(as.data.frame(rbindlist(clusters)))
}

itt<-5
avgDistance<-data.frame(K = numeric(0), Distance = numeric(0))
for (i in 1:itt){
  print(paste("K=",i))
  Final_Cluster<-my_K_Means(myData,i)
  avgDistance<-rbind(avgDistance,data.frame(K = i,Distance = avg_Cluster_Distance))
}

plot(avgDistance,type = "o",main = paste("Average Distance for k: 1-", itt))

Final_Cluster