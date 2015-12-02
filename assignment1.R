library(ggplot2)
library(scales)
library(ggmap)
library(lubridate)
library(plotrix)
car.data <- read.table(file = "C:/Users/MaGesh/Desktop/CS 591- Big data/Challenge 0/02-visualization/02-visualization/R-vis-files/data/uber_gps_tsv/all.tsv", sep="\t",  header=FALSE)

car.data$V2<-strptime(car.data$V2,format="%Y-%m-%dT%H:%M:%S")
str(car.data)
car.data_total_day<- subset(car.data,hour(car.data$V2) >= 6 & hour(car.data$V2)<=18)


car.data_night3<-subset(car.data,hour(car.data$V2)>18 & hour(car.data$V2)< 24)
car.data_night4<-subset(car.data,hour(car.data$V2)< 6 & hour(car.data$V2)> 0)
car.data_total_night<-rbind(car.data_night3,car.data_night4)

car.data <- droplevels(subset(car.data, V3 > 37.25 & V4 < 121.5))

car.map <- get_map(location = c(min(car.data$V4), min(car.data$V3), max(car.data$V4), max(car.data$V3)), maptype=997,api_key=api_key, source="osm")

ggmap(car.map)
ggmap(car.map) + geom_path(data=car.data_total_day, aes(x=V4, y=V3,group=V1), color="dark green") 
ggmap(car.map) + geom_path(data=car.data_total_night, aes(x=V4, y=V3,group=V1), color="red")
ggmap(car.map) + geom_path(data=car.data_total_day, aes(x=V4, y=V3,group=V1), color="dark green") + geom_path(data=car.data_total_night, aes(x=V4, y=V3,group=V1), color="red")

day_value=nrow(car.data_total_day)
day_value
night_value=nrow(car.data_total_night)
day_percentage=(day_value/(day_value+night_value))*100
night_percentage=(night_value/(day_value+night_value))*100
day_percentage<-round(day_percentage)
night_percentage<-round(night_percentage)
slices<-c(day_percentage,night_percentage)
labels<-c(slices[1],slices[2])
cols<-c("green","red")
pie3D(slices, main="DAYS vs NIGHT", col=cols,cex=0.8, labels=labels,explode=0.1)
legend("topright", c("Days %","Nights %"), cex=0.8, fill=cols)

