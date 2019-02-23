Components<-function(myData){
  components<-list()
  for (i in 1:ncol(myData)){
    components[[i]]<-myData[,i]
  }
  return (components)
}
