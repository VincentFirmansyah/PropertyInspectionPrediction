library(dplyr)
library(ggplot2)
library(randomForest)
library(foreach)
library(doParallel)
library(gbm)
library(caret)
#function got calculating normalized gini
source('NG.R')
#function for encoding categorical features into binary ones
source('encode.R')


data<-read.csv('train.csv')
test<-read.csv('test.csv')







train<-sample(nrow(data),nrow(data)*0.7)
traindata<-data[train,]
valdata<-data[-train,]

gbm_model= gbm(Hazard~.-Id, 
          data=traindata,
          distribution = "gaussian",
          n.trees = 100,
          interaction.depth = 10,
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


