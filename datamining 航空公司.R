setwd("C:\\Users\\feizhe\\Desktop\\HW2")
airline<-read.csv("AirlineData.csv",header=T)
names(airline)
#客户数据库中有三个神奇的要素，这三个要素构成了数据分析最好的指标：
#最近一次消费(Recency)
#消费频率(Frequency)
#消费金额(Monetary)
modelname<-c('FFP_DATE','AVG_INTERVAL','FLIGHT_COUNT','SEG_KM_SUM','avg_discount','LOAD_TIME')
data<-airline[,modelname]
summary(data) #存在0值和NA
index=(data$AVG_INTERVAL==0|data$avg_discount==0)
ddata<-data[-which(index==1),] #删除了平均乘机时间间隔和平均折扣率为0的数据
str(ddata)
ddata$FFP_DATE<-as.Date(ddata$FFP_DATE) #转换为日期格式
ddata$LOAD_TIMEE<-as.Date(ddata$LOAD_TIME)
str(ddata)
data_numeric<- transform(ddata,L=difftime(LOAD_TIME,FFP_DATE, units = 'days')/30)
str(data_numeric)
data_numeric$L<-as.numeric(data_numeric$L)
summary(data_numeric)
standard <- function(x) {
  (x-min(x))/(max(x)-min(x))
}
dddata<-data_numeric[,c(2,3,4,5,8)]
standarddata<-sapply(X=dddata,FUN=standard)
colnames(standarddata)=c("ZA","ZB","ZC","ZD","ZE")
head(zsredfile)
set.seed(1235)
result<- kmeans(x =standarddata, centers = 5)
type=result$cluster
table(type)
library(fpc)
plotcluster(standarddata,result$cluster)

library(fmsb)
centervec=result$center
max <- apply(centervec, 2, max)
min <- apply(centervec,2,min)
df = data.frame(rbind(max,min,centervec))
radarchart(df = df, seg=5, plty=1,vlcex=0.7)
