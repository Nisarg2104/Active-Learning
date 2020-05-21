library(readxl)
vehicle_dataset <- read_excel("C:/Users/Khushi/Desktop/Studies 2-2/Machine Learning Assignment/Active Learning Assignment/vehicle_dataset.xlsx")
str(vehicle_dataset)
vehicle_dataset$Class<-as.factor(vehicle_dataset$Class)
##vehicle_dataset[,1:18]<-scale(vehicle_dataset[,1:18],TRUE,TRUE)

library(dplyr)
library(caTools)
set.seed(12345)


s1 <-sample.split(vehicle_dataset$Class,SplitRatio = 0.90)
t1<-cbind(vehicle_dataset,s=s1)
unlabelledData<-subset(t1,s==TRUE)[1:18]
unlabelledData<-add_rownames(unlabelledData,"id")
unlabelledData<-sapply(unlabelledData,as.numeric)
labelledData<-subset(t1,s==FALSE)[1:19]




##clustering
train_set<-unlabelledData[sample(nrow(unlabelledData),nrow(unlabelledData)*0.4),]
#number of clusters
k<-4
train_set1<-train_set

#selecting 4 random centroids

maxChanges<-3


o<-0

c<-train_set[sample(nrow(train_set),4),]
for(j in c(1:20)){
  no_changes<-0
  if(j!=1){
    b4<-train_set$cl
  }
  train_set<-train_set[,1:19]
  Dc<-c(rowSums(sweep(train_set[,2:19],2,c[1,2:19])*sweep(train_set[,2:19],2,c[1,2:19])))
  cl<-c(rep(1,nrow(train_set)))
  train_set<-cbind.data.frame(train_set,Dc=Dc)
  train_set<-cbind.data.frame(train_set,cl)
  for(i in c(2:4)){
    Dc<-rowSums(sweep(train_set[,2:19],2,c[i,2:19])*sweep(train_set[,2:19],2,c[i,2:19]))
    b4<-train_set$cl
    train_set$cl<-if_else(train_set$Dc>Dc,i,as.integer(train_set$cl))
    train_set$Dc<-if_else(train_set$Dc>Dc,Dc,train_set$Dc)
  }
  aft<-train_set$cl
  no_changes<-if_else(b4==aft,0,1)
  no_changes<-sum(no_changes)
  
  for(i in c(1:4)){
    c[i,]<-colMeans(select(filter(select(train_set,-Dc),cl==i),-cl))
  }
  print(sum(train_set$Dc))
  
}


cluster1<-select(filter(select(train_set,-Dc),cl==1),-cl)
cluster2<-select(filter(select(train_set,-Dc),cl==2),-cl)
cluster3<-select(filter(select(train_set,-Dc),cl==3),-cl)
cluster4<-select(filter(select(train_set,-Dc),cl==4),-cl)

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


