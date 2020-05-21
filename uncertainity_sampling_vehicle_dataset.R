library(readxl)
library(caret)
vehicle_dataset <- read_excel("C:/Users/Khushi/Desktop/Studies 2-2/Machine Learning Assignment/Active Learning Assignment/vehicle_dataset.xlsx")
str(vehicle_dataset)
vehicle_dataset$Class<-as.factor(vehicle_dataset$Class)
vehicle_dataset[,1:18]<-scale(vehicle_dataset[,1:18],TRUE,TRUE)

library(dplyr)
library(caTools)
set.seed(1234567)
s1 <-sample.split(vehicle_dataset$Class,SplitRatio = 0.90)
t1<-cbind(vehicle_dataset,s=s1)
unlabelledData<-subset(t1,s==TRUE)[1:19]
labelledData<-subset(t1,s==FALSE)[1:19]
unlabelledData1<-unlabelledData
labelledData1<-labelledData
library(MASS)

model<-lda(Class~.,data=labelledData)
labelledData.lda<-predict(model,newdata =as.data.frame(labelledData) )$x
unlabelledData.lda<-predict(model,newdata = as.data.frame(unlabelledData[,1:18]))$x
labelledData.lda<-cbind.data.frame(labelledData.lda,"Class"=labelledData$Class)
unlabelledData.lda<-cbind.data.frame(unlabelledData.lda,"Class"=unlabelledData$Class)
library(naivebayes)

##Least Confidence
for(i in c(1:(nrow(vehicle_dataset)/2.5))){
  nb<-naive_bayes(Class~.,labelledData.lda)
  ##predict(nb,unlabelledData, type="class")
  print(confusionMatrix( predict(nb,unlabelledData.lda, type="class"),unlabelledData.lda$Class))
  a<-predict(nb,unlabelledData.lda, type="prob")
  unlabelledData.lda<-cbind(unlabelledData.lda,"max"=apply(a, 1, max))
  unlabelledData.lda<-unlabelledData.lda %>% arrange(max)
  labelledData.lda<-rbind(labelledData.lda,unlabelledData.lda[1,1:4])
  unlabelledData.lda<-unlabelledData.lda[2:nrow(unlabelledData.lda),1:4]
  print(i)
}



#MarginSampling
for(i in c(1:(nrow(vehicle_dataset)/2.5))){
  nb<-naive_bayes(Class~.,labelledData1)
  ##predict(nb,unlabelledData, type="class")
  print(confusionMatrix( predict(nb,unlabelledData1, type="class"),unlabelledData1$Class))
  a1<-predict(nb,unlabelledData1, type="prob")
  maxes<-t(sapply(1:nrow(a1),function(i){
    sort(a1[i,1:4],decreasing = TRUE)[1:2]
  }))
  diff<-maxes[,1]-maxes[,2]
  unlabelledData1<-cbind(unlabelledData1,"diff"=diff)
  unlabelledData1<-unlabelledData1 %>% arrange(diff)
  labelledData1<-rbind(labelledData1,unlabelledData1[1,1:19])
  unlabelledData1<-unlabelledData1[2:nrow(unlabelledData1),1:19]
  print(i)
}