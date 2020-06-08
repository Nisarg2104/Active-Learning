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
percent=0.10
i<-0
j<-1
nb<-naive_bayes(Class~.,labelledData.lda)

#Least Confidence
while (i<as.integer(percent*nrow(vehicle_dataset))&& j<=nrow(unlabelledData.lda)) {
  point<-unlabelledData.lda[j,]
  a<-predict(nb,point, type="prob")
  if(apply(a, 1, max)<0.78){
    labelledData.lda<-rbind(labelledData.lda,point)
    nb<-naive_bayes(Class~.,labelledData.lda)
    i<-i+1
    print(i)
  }
 j<-j+1  
}

#Margin Sampling
while (i<as.integer(percent*nrow(vehicle_dataset))&& j<=nrow(unlabelledData.lda)) {
  point<-unlabelledData.lda[j,]
  a<-predict(nb,point, type="prob")
  maxes<-sort(a,decreasing = TRUE)[1:2]
  diff<-maxes[1]-maxes[2]
  if(diff<0.70){
    labelledData.lda<-rbind(labelledData.lda,point)
    nb<-naive_bayes(Class~.,labelledData.lda)
    i<-i+1
    print(i)
  }
  j<-j+1  
}


#Entropy Sampling
while (i<as.integer(percent*nrow(vehicle_dataset))&& j<=nrow(unlabelledData.lda)) {
  point<-unlabelledData.lda[j,]
  a<-predict(nb,point, type="prob")
  a.log<-log2(a)
  if(rowSums(-a.log * a)>0.70){
    labelledData.lda<-rbind(labelledData.lda,point)
    nb<-naive_bayes(Class~.,labelledData.lda)
    i<-i+1
    print(i)
  }
  j<-j+1  
}

unlabelledData.lda<-setdiff(unlabelledData.lda, labelledData.lda)
nb<-naive_bayes(Class~.,labelledData.lda)
xtab<-table(predict(nb,unlabelledData.lda, type="class"),unlabelledData.lda$Class)
acc=sum(diag(xtab))/nrow(unlabelledData.lda)*100
print(acc)






