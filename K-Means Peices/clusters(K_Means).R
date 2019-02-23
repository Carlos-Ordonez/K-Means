Clusters<-function(Centroids){
  clusters<-list()
  for(i in 1:k){
    clusters[[i]]<-Centroids[i,]
    print(i)
  }
  return(clusters)
}