encode<-function(data){
  
  dummies<-dummyVars(~.,data=data)
  data = predict(dummies, newdata = data)
  data<-as.data.frame(data)
  
  return(data)
  
}