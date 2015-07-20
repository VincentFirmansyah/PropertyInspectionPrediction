library(dplyr)
library(ggplot2)
library(randomForest)
library(foreach)
library(doParallel)
library(gbm)
library(caret)
library(xgboost)
#function got calculating normalized gini
source('NG.R')
#function for encoding categorical features into binary ones
source('encode.R')
source('EncodeOrdinal.R')


data<-read.csv('train.csv')
test<-read.csv('test.csv')
# 



ggplot(data,aes(x=log(Hazard)))+
  geom_histogram(binwidth=0.5, colour="black", fill="white")+
  facet_grid(T1_V7~.)


ggplot(data,aes(x=log(Hazard)))+
  geom_histogram(binwidth=0.5, colour="black", fill="white")+
  facet_grid(T2_V12~.)

ggplot(data,aes(x=log(Hazard)))+
  geom_histogram(binwidth=0.5, colour="black", fill="white")+
  facet_grid(T2_V6~.)

data%>%
  group_by(T1_V9)%>%
  summarize(median(Hazard))

train<-sample(nrow(data),nrow(data)*0.7)
traindata<-data[train,]
valdata<-data[-train,]

#########gbm

gbm_model= gbm(Hazard~.-Id, 
          data=traindata,
          distribution = "gaussian",
          n.trees = 200,
          interaction.depth = 8,
          n.minobsinnode = 1,
          shrinkage = 0.2,
          bag.fraction = 0.8)


gbm.perf(gbm_model,
         plot.it=TRUE,
         oobag.curve=TRUE,
         overlay=TRUE,
         method='OOB')



pred_gbm=predict(gbm_model,valdata[,-1],n.trees=100,type="response")


NormalizedGini(valdata$Hazard,pred_gbm)




pred_gbm_submit=predict(gbm_model,test[,-1],n.trees=100,type="response")


Submission=data.frame("Id"=test[,1],"Hazard"=pred_gbm_submit)
write.csv(Submission,"Submission2.csv",row.names=FALSE,quote=FALSE)

############xgboost

data<-read.csv('train.csv')
test<-read.csv('test.csv')
set.seed(1234)
# 

data%>%
  group_by(T1_V7)%>%
  summarize(medavg=median(Hazard))

FeaEng<-function(data){
  
  
  T1_V7<-EncOrd(data$T1_V7,c(1,3,4,2))
  T1_V8<-EncOrd(data$T1_V8,c(3,2,4,1))
  T1_V12<-EncOrd(data$T1_V12,c(2,3,4,1))
  T1_V9<-EncOrd(data$T1_V9,c(2,1,3,5,5,4))
  
  data$Ord<-(T1_V7==4) + (T1_V8==4)+(T1_V12==4)+(T1_V9==4)
  
  #data$Ordmean<-(T1_V7+T1_V8+T1_V12+T1_V9)/4
  #data$Ordmult<-T1_V7*T1_V8*T1_V12*T1_V9
  
  return(data)
}



data<-FeaEng(data)

y=data[,2]
data<-data[,-c(1,2)]
dummies<-dummyVars(~.,data=data)
data= predict(dummies, newdata = data)


test<-FeaEng(test)
ID<-test[,1]
test<-test[,-1]
dummies2<-dummyVars(~.,data=test)
test= predict(dummies2, newdata = test)


train<-sample(nrow(data),nrow(data)*0.7)
traindata<-data[train,]
valdata<-data[-train,]

param <- list("objective" = "reg:linear",
              "eta"=0.05,
              min_child_weight=10,
              subsample=0.8,
              colsample_bytree=.8,
              scale_pos_weight=1.0,
              max_depth=8,
              "nthread" = 4, 
              "verbose"=0)


xgb_model = xgboost(param=param, 
                  data = traindata, 
                  label = y[train],
                  nrounds=500) 


predict_xgboost <- predict(xgb_model, valdata)
NormalizedGini(y[-train],predict_xgboost)

rf_model<-randomForest(y=y[train],
                    x=traindata,
                    ntree=170, 
                    imp=TRUE, 
                    sampsize=10000, 
                    do.trace=TRUE)

predict_rf <- predict(rf_model, valdata)
NormalizedGini(y[-train],predict_rf)

predict_xgboost <- predict(xgb_model, traindata)
predict_rf <- predict(rf_model, traindata)

lm_stack<-lm(y[train]~predict_xgboost+predict_rf)


NormalizedGini(y[-train],(predict_rf*0.5+predict_xgboost*0.5))


imptmat<-xgb.importance(colnames(traindata),model=xgb_model)
xgb.plot.importance(imptmat)
arrange(as.data.frame(imptmat),desc(Gain))

result_xgboost <- predict(xgb_model, test)
result_rf<-predict(rf_model,test)

result<-(result_xgboost+result_rf)/2

Submission=data.frame("Id"=ID,"Hazard"=result)
write.csv(Submission,"Submission6_ensemble.csv",row.names=FALSE,quote=FALSE)





lm_model<-lm(Hazard~.,traindata[,2:ncol(traindata)])
lm_pred<-predict(lm_model,valdata[,2:ncol(valdata)])
NormalizedGini(valdata$Hazard,lm_pred)


  
cl <- makeCluster(4)
registerDoParallel(cl)

rf_model<-randomForest(Hazard~.,data=traindata[,2:ncol(traindata)],importance=TRUE,ntree=100)


rf_model  <-  foreach(iteration=1:4,ntree=rep(50,  4),  
                      .combine=combine, 
                      #.multicombine=TRUE,
                      .packages=c('randomForest'))  %dopar% {
                        sink("rflog.txt", append=TRUE)
                        cat(paste("Starting iteration",iteration,"\n"))
                        randomForest(Hazard~.,data=traindata[,2:ncol(traindata)],  
                                     ntree=ntree,do.trace=1)
                      }


