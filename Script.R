library(dplyr)
library(ggplot2)
library(randomForest)
library(foreach)
library(doParallel)
source('NG.R')




data<-read.csv('train.csv')



train<-sample(nrow(data),nrow(data)*0.7)
traindata<-data[train,]
valdata<-data[-train,]


lm_model<-lm(Hazard~.,traindata[,2:ncol(traindata)])
lm_pred<-predict(lm_model,valdata[,2:ncol(valdata)])

NormalizedGini(valdata$Hazard,lm_pred)


cl <- makeCluster(4)
registerDoParallel(cl)

rf_model<-randomForest(Hazard~.,data=traindata[,2:ncol(traindata)],mtry=5,importance=TRUE)


rf_model  <-  foreach(iteration=1:4,ntree=rep(150,  4),  
                      .combine=combine, 
                      #.multicombine=TRUE,
                      .packages=c('randomForest'))  %dopar% {
                        sink("rflog.txt", append=TRUE)
                        cat(paste("Starting iteration",iteration,"\n"))
                        randomForest(formRF,data=traindata[,2:ncol(traindata)],  
                                     ntree=ntree,do.trace=1)
                      }


