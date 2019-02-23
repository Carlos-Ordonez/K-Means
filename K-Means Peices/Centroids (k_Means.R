Centroids<-function(myDataComponents,k){
  Centroids<-list()
  for(i in 1:length(myDataComponents)){
    Centroids[[i]]<-myDataComponents[[i]][int_Centroid_Index[1:k],]
    print(i)
  }
  return (Centroids)
}