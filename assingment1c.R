library(ggplot2)
library(scales)
library(ggmap)
library(lubridate)
library(dplyr)
library(plyr)

car.duration <- read.table(file = "C:/Users/MaGesh/Desktop/CS 591- Big data/Challenge 0/02-visualization/02-visualization/R-vis-files/data/uber_gps_tsv/all.tsv", sep="\t",  header=FALSE)

car.duration$V2<-strptime(car.duration$V2,format="%Y-%m-%dT%H:%M:%S")

car.duration_day<- subset(car.duration,hour(car.duration$V2) >= 6 & hour(car.duration$V2)<=18)

car.data_night3<-subset(car.duration,hour(car.duration$V2)>18 & hour(car.duration$V2)< 24)
car.data_night4<-subset(car.duration,hour(car.duration$V2)< 6 & hour(car.duration$V2)> 0)
car.data_total_night<-rbind(car.data_night3,car.data_night4)

car.count_night<-count(car.data_total_night$V1)

car.count1<-count(car.duration_day$V1)


#day_top
freq_values<-car.count1[order(car.count1$freq,decreasing=TRUE),]
freq_values_top<- droplevels(subset(freq_values, freq> 229))

#day_least
freq_values_dayAsc<-car.count1[order(car.count1$freq),]
freq_values_least<- droplevels(subset(freq_values_dayAsc, freq > 1))

#night_top
freq_values_nightDesc<-car.count_night[order(car.count_night$freq,decreasing=TRUE),]
freq_values_nighttop<- droplevels(subset(freq_values_nightDesc, freq> 296))


#night_least
freq_values_nightAsc<-car.count_night[order(car.count_night$freq),]
freq_values_nightleast<- droplevels(subset(freq_values_nightAsc, freq> 1))


car.data <- read.table(file = "C:/Users/MaGesh/Desktop/CS 591- Big data/Challenge 0/02-visualization/02-visualization/R-vis-files/data/uber_gps_tsv/all.tsv", sep="\t",  header=FALSE)

car.data$V2 <- NULL

car.data <- droplevels(subset(car.data, V3 > 37.25 & V4 < 121.5))

#car.map <- get_map(location = c(min(car.data$V4), min(car.data$V3), max(car.data$V4), max(car.data$V3)), maptype="toner", source="stamen")
car.map <- get_map(location = c(min(car.data_days$V4), min(car.data_days$V3), max(car.data_days$V4), max(car.data_days$V3)), maptype=997,api_key=api_key, source="osm")
ggmap(car.map)

car.data_night31111<-subset(car.data, V1 == 13135)
car.data_night3_3<-subset(car.data, V1 == 240)
car.data_night3_4<-subset(car.data, V1 == 2597)
car.data_night3_5<-subset(car.data, V1 == 43)

#day_top
ggmap(car.map) + geom_path(data=subset(car.data, V1 == 13135), aes(x=V4, y=V3), color="red") + geom_path(data=subset(car.data, V1 == 4816), aes(x=V4, y=V3), color="red") + geom_path(data=subset(car.data, V1 == 14760), aes(x=V4, y=V3), color="red") + geom_path(data=subset(car.data, V1 == 9697), aes(x=V4, y=V3), color="red")+ geom_path(data=subset(car.data, V1 == 11721), aes(x=V4, y=V3), color="red")

#day_least
ggmap(car.map) + geom_point(data=subset(car.data, V1 == 240), aes(x=V4, y=V3), color="blue") + geom_point(data=subset(car.data, V1 == 441), aes(x=V4, y=V3), color="blue") + geom_point(data=subset(car.data, V1 == 1036), aes(x=V4, y=V3), color="blue") + geom_point(data=subset(car.data, V1 == 1175), aes(x=V4, y=V3), color="blue")+ geom_point(data=subset(car.data, V1 == 19), aes(x=V4, y=V3), color="blue")

#night_top
ggmap(car.map) + geom_path(data=subset(car.data, V1 == 2597), aes(x=V4, y=V3), color="green") + geom_path(data=subset(car.data, V1 == 21629), aes(x=V4, y=V3), color="green") + geom_path(data=subset(car.data, V1 == 4110), aes(x=V4, y=V3), color="green") + geom_path(data=subset(car.data, V1 == 5779), aes(x=V4, y=V3), color="green")+ geom_path(data=subset(car.data, V1 == 23251), aes(x=V4, y=V3), color="green")

#night_least
#ggmap(car.map) + geom_path(data=subset(car.data, V1 == 43), aes(x=V4, y=V3), color="yellow") + geom_path(data=subset(car.data, V1 == 71), aes(x=V4, y=V3), color="yellow") + geom_path(data=subset(car.data, V1 == 273), aes(x=V4, y=V3), color="yellow") + geom_path(data=subset(car.data, V1 == 284), aes(x=V4, y=V3), color="yellow")+ geom_path(data=subset(car.data, V1 == 493), aes(x=V4, y=V3), color="yellow")
ggmap(car.map) + geom_point(data=subset(car.data, V1 == 43), aes(x=V4, y=V3), color="yellow") + geom_point(data=subset(car.data, V1 == 71), aes(x=V4, y=V3), color="yellow") + geom_point(data=subset(car.data, V1 == 273), aes(x=V4, y=V3), color="yellow") + geom_point(data=subset(car.data, V1 == 284), aes(x=V4, y=V3), color="yellow")+ geom_point(data=subset(car.data, V1 == 493), aes(x=V4, y=V3), color="yellow")

#night&day_top
ggmap(car.map) + geom_path(data=subset(car.data, V1 == 13135), aes(x=V4, y=V3), color="red") + geom_path(data=subset(car.data, V1 == 4816), aes(x=V4, y=V3), color="red") + geom_path(data=subset(car.data, V1 == 14760), aes(x=V4, y=V3), color="red") + geom_path(data=subset(car.data, V1 == 9697), aes(x=V4, y=V3), color="red")+ geom_path(data=subset(car.data, V1 == 11721), aes(x=V4, y=V3), color="red")+geom_path(data=subset(car.data, V1 == 2597), aes(x=V4, y=V3), color="green") + geom_path(data=subset(car.data, V1 == 21629), aes(x=V4, y=V3), color="green") + geom_path(data=subset(car.data, V1 == 4110), aes(x=V4, y=V3), color="green") + geom_path(data=subset(car.data, V1 == 5779), aes(x=V4, y=V3), color="green")+ geom_path(data=subset(car.data, V1 == 23251), aes(x=V4, y=V3), color="green")

#daynight_least
ggmap(car.map) + geom_point(data=subset(car.data, V1 == 240), aes(x=V4, y=V3), color="blue") + geom_point(data=subset(car.data, V1 == 441), aes(x=V4, y=V3), color="blue") + geom_point(data=subset(car.data, V1 == 1036), aes(x=V4, y=V3), color="blue") + geom_point(data=subset(car.data, V1 == 1175), aes(x=V4, y=V3), color="blue")+ geom_point(data=subset(car.data, V1 == 19), aes(x=V4, y=V3), color="blue") + geom_point(data=subset(car.data, V1 == 43), aes(x=V4, y=V3), color="yellow") + geom_point(data=subset(car.data, V1 == 71), aes(x=V4, y=V3), color="yellow") + geom_point(data=subset(car.data, V1 == 273), aes(x=V4, y=V3), color="yellow") + geom_point(data=subset(car.data, V1 == 284), aes(x=V4, y=V3), color="yellow")+ geom_point(data=subset(car.data, V1 == 493), aes(x=V4, y=V3), color="yellow")
