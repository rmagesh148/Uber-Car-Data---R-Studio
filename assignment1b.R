library(ggplot2)
library(scales)
library(ggmap)
library(lubridate)

car.data_days <- read.table(file = "C:/Users/MaGesh/Desktop/CS 591- Big data/Challenge 0/02-visualization/02-visualization/R-vis-files/data/uber_gps_tsv/all.tsv", sep="\t",  header=FALSE)


car.data_days$V2<-strptime(car.data_days$V2,format="%Y-%m-%dT%H:%M:%S")
str(car.data_days)

car.data_days$week_day<-wday(car.data_days$V2)

car.data_days <- droplevels(subset(car.data_days, V3 > 37.25 & V4 < 121.5))

car.map_days <- get_map(location = c(min(car.data_days$V4), min(car.data_days$V3), max(car.data_days$V4), max(car.data_days$V3)), maptype=997,api_key=api_key, source="osm")

weekend_value=subset(car.data_days,week_day==1|week_day==7)
weekday_value=subset(car.data_days,week_day==2|week_day==3|week_day==4|week_day==5|week_day==6)

ggmap(car.map_days)
ggmap(car.map_days) + geom_path(data=subset(car.data_days,week_day==1|week_day==7), aes(x=V4, y=V3,group=V1), color="red")
ggmap(car.map_days) + geom_path(data=subset(car.data_days,week_day==2|week_day==3|week_day==4|week_day==5|week_day==6), aes(x=V4, y=V3,group=V1), color="dark green")
ggmap(car.map_days) + geom_path(data=subset(car.data_days,week_day==1|week_day==7), aes(x=V4, y=V3,group=V1), color="red")+ geom_path(data=subset(car.data_days,week_day==2|week_day==3|week_day==4|week_day==5|week_day==6), aes(x=V4, y=V3,group=V1), color="dark green")

weekend_count=nrow(weekend_value)
weekday_count=nrow(weekday_value)
weekend_percentage=(weekend_count/(weekend_count+weekday_count))*100
weekday_percentage=(weekday_count/(weekend_count+weekday_count))*100

weekend_percentage<-round(weekend_percentage)
weekday_percentage<-round(weekday_percentage)
values<-c(weekend_percentage,weekday_percentage)
labels<-c(values[1],values[2])

cols<-c("yellow","blue")
pie3D(values, main="Weekends vs Weekdays", col=cols, labels=labels, cex=0.8,explode=0.1)
legend("topright", c("Weekends %","Weekdays %"), cex=0.8, fill=cols)

