library(factoextra)
library(readxl)
vehicle_dataset <- read_excel("C:/Users/Khushi/Desktop/Studies 2-2/Machine Learning Assignment/Active Learning Assignment/vehicle_dataset.xlsx")
str(vehicle_dataset)
vehicle_dataset$Class<-as.factor(vehicle_dataset$Class)


library(dplyr)
library(caTools)


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
df<-train_set[,2:19]

km.res <- kmeans(df, 4, nstart = 25)
print(km.res)

dd <- cbind(train_set, cl = km.res$cluster)

cluster1<-select(filter(as.data.frame(dd),cl==1),-cl)
cluster2<-select(filter(as.data.frame(dd),cl==2),-cl)
cluster3<-select(filter(as.data.frame(dd),cl==3),-cl)
cluster4<-select(filter(as.data.frame(dd),cl==4),-cl)

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