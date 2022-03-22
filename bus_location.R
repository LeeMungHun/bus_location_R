library(devtools)
library(ggmap)
library(XML)
library(dplyr)
library(tidyr)

#API키 입력하기
API_key <- ""

#노선정보조회서비스에서 버스번호에 해당하는  노선ID값 가져오기
busRtNm <- "402"
url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getBusRouteList?ServiceKey=", API_key,
             "&strSrch=" , busRtNm,sep="")
xmefile <- xmlParse(url) 
xmlRoot(xmefile) 
df <- xmlToDataFrame(getNodeSet(xmefile,"//itemList"))
df_busRoute <- subset(df,busRouteNm== busRtNm)

print(paste(busRtNm,"번 버스의 노선ID=",df_busRoute$busRouteId ))


#버스위치정보 조회API에서 노선ID로 버스위치,차량번호 가져오기
url2 <- paste("http://ws.bus.go.kr/api/rest/buspos/getBusPosByRtid?ServiceKey=",API_key,"&busRouteId=",df_busRoute$busRouteId,sep="")

xmefile2 <- xmlParse(url2)
xmlRoot(xmefile2)

df2 <- xmlToDataFrame(getNodeSet(xmefile2,"//itemList"))
#위,경도,차량번호
gpsX <- as.numeric(as.character(df2$gpsX))
gpsY <- as.numeric(as.character(df2$gpsY))
plainNo <- as.character(df2$plainNo)#차량번호
#각 버스에 대한 위치정보를 데이터프레임으로 만들기
gc <- data.frame(lon = gpsX ,lat= gpsY)
df_imsi<- data.frame(plainNo,lon = gpsX ,lat= gpsY)

#차량이 너무 많아서 1/3으로 줄이기
gc <-gc[c(TRUE, FALSE, FALSE), ]
df_imsi <- df_imsi[c(TRUE, FALSE, FALSE), ]

#노선ID로 모든 정류소의 도착예정 버스정보 가져오기 정류소값 가져오기
url3 <- paste("http://ws.bus.go.kr/api/rest/arrive/getArrInfoByRouteAll?serviceKey=",API_key,"&busRouteId=",df_busRoute$busRouteId,sep="")
xmefile3 <- xmlParse(url3)
xmlRoot(xmefile3)

df3 <- xmlToDataFrame(getNodeSet(xmefile3,"//itemList"))

#벡터와 데이터프레임 변수 생성
imsi_c <- c()
df4 <- data.frame()
# 다음 정류소 데이터프레임 생성
for(i in plainNo){
  imsi_c <-select(df3,plainNo1,stationNm1) %>% filter(df3$plainNo1 == i)%>% unique()
  df4 <- rbind(df4,imsi_c)
  
}
df4

#차량번호, 위경도,다음정류소 값을 하나의 데이터프레임으로 생성
#정류소의 도착정보와 버스위치,번호 정보를 innerjoin
df4<- rename(df4,"plainNo" ="plainNo1")
result<-inner_join(df4,df_imsi,by='plainNo')


#구글맵 시각화
googleAPIKey <- "AIzaSyAM32aiT9MG4q08-Lz6U0AUs-7VkavfMMw"
register_google(key=googleAPIKey)

#가운데 값 구하기 Center
cen <- c(mean(result$lon),mean(result$lat))

#maptype 지형,위성,로드맵 .....
map <- get_googlemap(center= cen , maptype = "roadmap",zoom = 12,markers = select(result,lon,lat))
gmap<-ggmap(map,extent = "device",legend = "topright")
gmap + geom_text(data= result , aes(x=lon,y=lat),size= 4, label=result$plainNo) + geom_text(data= result , aes(x=lon,y=lat,color = factor(paste(c(1:nrow(result)),stationNm1))),alpha=0,size= 4, label=paste("(",as.character(c(1:nrow(result))),")") , hjust=0, vjust=-3)+ geom_text(data= result , aes(x=lon,y=lat),size= 4, label=paste("(",as.character(c(1:nrow(result))),")") , hjust=0, vjust=-3) +labs(colour=paste(busRtNm,"번 버스 위치"))

#사진에 맞게 위치 조정
setwd("/temp")
png(filename = "402bus.png", width = 480, height = 480)
gmap + geom_text(data= result , aes(x=lon,y=lat),size= 4, label=result$plainNo) + geom_text(data= result , aes(x=lon,y=lat,color = factor(paste(c(1:nrow(result)),stationNm1))),alpha=0,size= 4, label=paste("(",as.character(c(1:nrow(result))),")") , hjust=0, vjust=-3)+ geom_text(data= result , aes(x=lon,y=lat),size= 4, label=paste("(",as.character(c(1:nrow(result))),")") , hjust=-0.5, vjust=-4) +labs(colour=paste(busRtNm,"번 버스 위치"))
dev.off()