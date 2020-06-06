library(readxl)
vehicle_dataset <- read_excel("C:/Users/Khushi/Desktop/Studies 2-2/Machine Learning Assignment/Active Learning Assignment/vehicle_dataset.xlsx")
str(vehicle_dataset)
vehicle_dataset$Class<-as.factor(vehicle_dataset$Class)
vehicle_dataset[,1:18]<-scale(vehicle_dataset[,1:18],TRUE,TRUE)

library(dplyr)
library(caTools)
set.seed(1234)
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
labelledData1.lda<-labelledData.lda
unlabelledData1.lda<-unlabelledData.lda
library(naivebayes)
percent=0.30
##Least Confidence
for(i in c(1:(nrow(vehicle_dataset)*percent))){
  nb<-naive_bayes(Class~.,labelledData.lda)
  ##predict(nb,unlabelledData, type="class")
  xtab<-table(predict(nb,unlabelledData.lda, type="class"),unlabelledData.lda$Class)
  acc=sum(diag(xtab))/nrow(unlabelledData.lda)*100
  print(acc)
  a<-predict(nb,unlabelledData.lda, type="prob")
  unlabelledData.lda<-cbind(unlabelledData.lda,"max"=apply(a, 1, max))
  unlabelledData.lda<-unlabelledData.lda %>% arrange(max)
  nrow(unlabelledData.lda)
  
  labelledData.lda<-rbind(labelledData.lda,unlabelledData.lda[1,1:4])
  unlabelledData.lda<-unlabelledData.lda[2:nrow(unlabelledData.lda),1:4]
  nrow(unlabelledData.lda)
  print(i)
}



#MarginSampling
for(i in c(1:(nrow(vehicle_dataset)*percent))){
  nb<-naive_bayes(Class~.,labelledData1.lda)
  ##predict(nb,unlabelledData, type="class")
  xtab<-table(predict(nb,unlabelledData1.lda, type="class"),unlabelledData1.lda$Class)
  acc=sum(diag(xtab))/nrow(unlabelledData1.lda)*100
  print(acc)
  a1<-predict(nb,unlabelledData1.lda, type="prob")
  maxes<-t(sapply(1:nrow(a1),function(i){
    sort(a1[i,1:4],decreasing = TRUE)[1:2]
  }))
  diff<-maxes[,1]-maxes[,2]
  unlabelledData1.lda<-cbind(unlabelledData1.lda,"diff"=diff)
  unlabelledData1.lda<-unlabelledData1.lda %>% arrange(diff)
  labelledData1.lda<-rbind(labelledData1.lda,unlabelledData1.lda[1,1:4])
  unlabelledData1.lda<-unlabelledData1.lda[2:nrow(unlabelledData1.lda),1:4]
  print(i)
}


#Entropy Sampling
for(i in c(1:(nrow(vehicle_dataset)*percent))){
  nb<-naive_bayes(Class~.,labelledData.lda)
  ##predict(nb,unlabelledData, type="class")
  xtab<-table(predict(nb,unlabelledData.lda, type="class"),unlabelledData.lda$Class)
  acc=sum(diag(xtab))/nrow(unlabelledData.lda)*100
  print(acc)
  a<-predict(nb,unlabelledData.lda, type="prob")
  a.log<-log2(a)
  unlabelledData.lda<-cbind(unlabelledData.lda,"entropy"=rowSums(-a.log * a))
  
  unlabelledData.lda<-unlabelledData.lda %>% arrange(entropy)
  labelledData.lda<-rbind(labelledData.lda,unlabelledData.lda[nrow(unlabelledData.lda),1:4])
  unlabelledData.lda<-unlabelledData.lda[1:(nrow(unlabelledData.lda)-1),1:4]
  print(i)
}
