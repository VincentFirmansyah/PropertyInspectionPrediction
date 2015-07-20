EncOrd<-function (data,level){
  
  levels(data)<-level
  data<-as.numeric(data)
  
  return(data)
  
}