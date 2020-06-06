library(readxl)
library(caret)
vehicle_dataset <- read_excel("C:/Users/Khushi/Desktop/Studies 2-2/Machine Learning Assignment/Active Learning Assignment/vehicle_dataset.xlsx")
str(vehicle_dataset)
vehicle_dataset$Class<-as.factor(vehicle_dataset$Class)
vehicle_dataset[,1:18]<-scale(vehicle_dataset[,1:18],TRUE,TRUE)


library(caTools)
set.seed(12345)


s1 <-sample.split(vehicle_dataset$Class,SplitRatio = 0.90)
t1<-cbind(vehicle_dataset,s=s1)
unlabelledData<-subset(t1,s==TRUE)[1:18]
unlabelledData<-add_rownames(unlabelledData,"id")
unlabelledData<-sapply(unlabelledData,as.numeric)
labelledData<-subset(t1,s==FALSE)[1:19]
library(MASS)

model<-lda(Class~.,data=labelledData)
labelledData.lda<-predict(model,newdata =as.data.frame(labelledData) )$x
unlabelledData.lda<-predict(model,newdata = as.data.frame(unlabelledData[,2:19]))$x
labelledData.lda<-cbind(labelledData.lda,labelledData$Class)

##clustering
train_set<-unlabelledData[sample(nrow(unlabelledData),nrow(unlabelledData)*0.4),]
train_set.lda<-predict(model,newdata = as.data.frame(train_set[,2:19]))$x
train_set.lda<-cbind(train_set.lda,"id"=train_set[,1])

#number of clusters
k<-4

#selecting 4 random centroids

c<-train_set.lda[sample(nrow(train_set.lda),4),]
library(dplyr)
for(j in c(1:20)){
  train_set.lda<-train_set.lda[,1:4]
  Dc<-c(rowSums(sweep(train_set.lda[,1:3],2,c[1,1:3])*sweep(train_set.lda[,1:3],2,c[1,1:3])))
  cl<-c(rep(1,nrow(train_set.lda)))
  train_set.lda<-cbind.data.frame(train_set.lda,Dc=Dc)
  train_set.lda<-cbind.data.frame(train_set.lda,cl)
  for(i in c(2:4)){
    Dc<-rowSums(sweep(train_set.lda[,1:3],2,c[i,1:3])*sweep(train_set.lda[,1:3],2,c[i,1:3]))
    
    train_set.lda$cl<-if_else(train_set.lda$Dc>Dc,i,as.integer(train_set.lda$cl))
    train_set.lda$Dc<-if_else(train_set.lda$Dc>Dc,Dc,train_set.lda$Dc)
  }
  
  
  for(i in c(1:4)){
    c[i,]<-colMeans(filter(as.data.frame(train_set.lda)[,-5],cl==i)[,-5])
  }
  print(sum(train_set.lda$Dc))
  
}






cluster1<-filter(train_set.lda[,-5],cl==1)[,-5]
cluster2<-filter(train_set.lda[,-5],cl==2)[,-5]
cluster3<-filter(train_set.lda[,-5],cl==3)[,-5]
cluster4<-filter(train_set.lda[,-5],cl==4)[,-5]

cluster1.20<-cluster1[sample(nrow(cluster1),nrow(cluster1)*0.2),]
vehicle_dataset<-add_rownames(vehicle_dataset,"id")
cluster1.20.labels<-filter(vehicle_dataset,as.numeric(vehicle_dataset$id) %in% cluster1.20$id)$Class

n1<-names(which.max(summary(cluster1.20.labels)))
cluster1.labels.actual<-filter(vehicle_dataset,as.numeric(vehicle_dataset$id) %in% cluster1$id)$Class



cluster2.20<-cluster2[sample(nrow(cluster2),nrow(cluster2)*0.2),]
vehicle_dataset<-add_rownames(vehicle_dataset,"id")
cluster2.20.labels<-filter(vehicle_dataset,as.numeric(vehicle_dataset$id) %in% cluster2.20$id)$Class

n2<-names(which.max(summary(cluster2.20.labels)))
cluster2.labels.actual<-filter(vehicle_dataset,as.numeric(vehicle_dataset$id) %in% cluster2$id)$Class




cluster3.20<-cluster3[sample(nrow(cluster3),nrow(cluster3)*0.2),]
vehicle_dataset<-add_rownames(vehicle_dataset,"id")
cluster3.20.labels<-filter(vehicle_dataset,as.numeric(vehicle_dataset$id) %in% cluster3.20$id)$Class

n3<-names(which.max(summary(cluster3.20.labels)))
cluster3.labels.actual<-filter(vehicle_dataset,as.numeric(vehicle_dataset$id) %in% cluster3$id)$Class

cluster4.20<-cluster4[sample(nrow(cluster4),nrow(cluster4)*0.2),]
vehicle_dataset<-add_rownames(vehicle_dataset,"id")
cluster4.20.labels<-filter(vehicle_dataset,as.numeric(vehicle_dataset$id) %in% cluster4.20$id)$Class

n4<-names(which.max(summary(cluster4.20.labels)))
cluster4.labels.actual<-filter(vehicle_dataset,as.numeric(vehicle_dataset$id) %in% cluster4$id)$Class


print(summary(cluster1.labels.actual)[n1])
print(summary(cluster2.labels.actual)[n2])
print(summary(cluster3.labels.actual)[n3])
print(summary(cluster4.labels.actual)[n4])
