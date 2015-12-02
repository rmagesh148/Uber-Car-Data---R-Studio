library(cluster)
library(sem)
library(Rserve)
library(dplyr)
library(gridBase)
library(grid)
library(fpc)
trainingdata<-read.table("C:/Users/MaGesh/Desktop/Neural Networks/TrainingData.txt",header =FALSE );
x<-head(max(trainingdata$V4))
x
max(trainingdata$V4)
trainingdata
trainingplot <-cbind(trainingdata$V3,trainingdata$V4)
trainingplot <- as.data.table(trainingplot)
trainingplot
head(trainingplot)
tclust <- kmeans(trainingplot,4,iter.max=1000)
tclust$centers   #$cluster to find number of clusters
plot(trainingdata[c("V3","V4")],col=tclust$cluster,main="Training Data for BackPropagation", ylab="x2",xlab = "x1");
