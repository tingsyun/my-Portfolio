Stress_Lysis<- read.csv("/Users/teresa/Desktop/Stress-Lysis.csv") 
head(Stress_Lysis)
View(Stress_Lysis)
#K-Means
Stress_Lysis2=Stress_Lysis[,-2]
set.seed(1234)
kmeans.result=kmeans(Stress_Lysis2,3)
kmeans.result
table(Stress_Lysis$Stress.Level,kmeans.result$cluster)
plot(Stress_Lysis2,col=kmeans.result$cluster)
kmeansoutput<-cbind(Stress_Lysis, cluster = kmeans.result$cluster) #rbind/cbind 合併資料
View(kmeansoutput)

#K-Medoids
Stress_Lysis2=Stress_Lysis[,-2]
install.packages('cluster')
library(cluster)
pam.result=pam(Stress_Lysis2,3) 
pam.result
table(Stress_Lysis$Stress.Level,pam.result$clustering)
layout(matrix(c(1,2),1,2))
plot(pam.result)
layout(matrix(1))
Medoidsoutput<-cbind(SStress_Lysis, cluster = pam.result$cluster) #rbind/cbind 合併資料
View(Medoidsoutput)

#K-Prototypes
install.packages('clustMixType')
library(clustMixType)
kpres <- kproto(Stress_Lysis, 3, lambda = 0.1)
clprofiles(kpres, Stress_Lysis)
plot(Stress_Lysis2,col=kpres$cluster)
Prototypesoutput<-cbind(Stress_Lysis, cluster = kpres$cluster) #rbind/cbind 合併資料
View(Prototypesoutput)

#K的數量
install.packages('NbClust')
library(NbClust)
Stress_Lysis2=Stress_Lysis[,-2]
result=NbClust(Stress_Lysis2,distance="euclidean",min.nc=2,max.nc=6,method="kmeans", index="all")
result$Best.partition

#手肘法
#應用在 K-Means
install.packages('ggplot2')
library(ggplot2)
install.packages('factoextra')
library(factoextra)
fviz_nbclust(Stress_Lysis2, 
             FUNcluster = kmeans,
             method = "wss",     
             k.max = 12          
) +
  
  labs(title="Elbow Method for K-Means") +
  
  geom_vline(xintercept = 3,      
             linetype = 2)      

# 應用在 K-Medoid
fviz_nbclust(Stress_Lysis2, 
             FUNcluster = pam,   
             method = "wss",     
             k.max = 12          
) +
  
  labs(title="Elbow Method for K-Medoid") +
  
  geom_vline(xintercept = 3,       
             linetype = 2)         

#輪廓係數
#應用在 K-Means
fviz_nbclust(StressLysis2, 
             FUNcluster = kmeans,   
             method = "silhouette", 
             k.max = 12             
) +
  
  labs(title="Avg.Silhouette Method for K-Means") 

#階層式集群Hierarchical
StressLysis2=Stress_Lysis[,-2]
index=sample(1:nrow(Stress_Lysis2),40) 
Stress_Lysissample=Stress_Lysis2[index,]
hclust.result=hclust(dist(Stress_Lysissample),method= 'ward.D2') 
hclust.result
plot(hclust.result,labels=Stress_Lysis$Stress.Level[index])
rect.hclust(hclust.result,k=3,border="red")
groups=cutree(hclust.result,k=3)
table(Stress_Lysis$Stress.Level[index],groups)

#密度基礎集群dbscan
Stress_Lysis2=Stress_Lysis[,-2]
library(fpc)
dbscan.result=dbscan(Stress_Lysis2,eps=0.01,MinPts=3)
dbscan.result
table(Stress_Lysis$Stress.Level, dbscan.result$cluster)
plot(dbscan.result,Stress_Lysis2)
plot(dbscan.result,Stress_Lysis2[c(1,4)])
plotcluster(Stress_Lysis2,dbscan.result$cluster)
dbscan.result$cluster
dbscanoutput<-cbind(Stress_Lysis, cluster = dbscan.result$cluster) 
dbscanoutput

#outlier
kmeans.result$centers
centers=kmeans.result$centers[kmeans.result$cluster,]
head(centers)
distances=sqrt(rowSums((Stress_Lysis2-centers)^2))
outliers=order(distances,decreasing=T)[1:5]
outliers
Stress_Lysis2[outliers,]
plot(Stress_Lysis2[c("Humidity", "Step.count")], col=kmeans.result$cluster)
points(kmeans.result$centers[,c("Humidity", "Step.count")], col=1:3, pch=4, cex=2)
points(Stress_Lysis2[outliers,c("Humidity", "Step.count")], col=4, pch='+', cex=2)
