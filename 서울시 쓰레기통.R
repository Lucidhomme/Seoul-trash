#웹 크롤링 연습

url2 <- "http://www.cgv.co.kr/movies/detail-view/?midx=83408#commentReg"
link2 <- read_html(url2, encoding = 'UTF-8')

movieexpect <- link2 %>%
  html_nodes("body") %>%
  html_nodes("#cgvwrap") %>%
  html_nodes("#contaniner") %>%
  html_nodes("#contents") %>%
  html_nodes(".wrap-movie-detail") %>%
  html_nodes(".cols-content") %>%
  html_nodes(".col-detail") %>%
  html_nodes(".sect-grade") %>%
  html_nodes(".wrap-persongrade")



#그래도 특문이나 공백등 떄문에 전처리가 필요
library(stringr)
lotto15 <- str_replace_all(lotto15, "\t|\n|\r","")
lotto15 <- str_replace_all(lotto15, "[[:space:]]","")

#1,2,3,...인덱스숫자가 의미가없어서 뺴려고 함.그래서 나머지 값들을 하나의 로우로 만들어서 루프를 돌릴것
storeName <- NULL
cnt       <- NULL
address   <- NULL

for(idx in 1:length(lotto15) ) {
  if(idx %% 5 == 2) {            ##상호이름이 로또테이블 가로줄 5개중에 2번째에 계속 들어있음
    storeName <- c(storeName, lotto15[idx])
  }else if(idx %% 5 ==3) {
    cnt <- c(cnt, lotto15[idx])
  }else if(idx %% 5 ==4) {
    address <- c(address, lotto15[idx])
  }
}
lottoDF <- data.frame(storeName,cnt,address)
#####첫페이지처리

last <- link %>%
  html_nodes('.paginate_common') %>%
  html_nodes('a') %>%
  html_attr('onclick') %>% tail(1)    ###왜 온클릭????
?html_attr
?gregexpr
?regmatches
end <- regmatches(last , gregexpr('[0-9]',last))
end <- as.numeric(end[[1]])
end <- as.numeric(paste(end[1],end[2],end[3],sep= ""))      ##이 과정 왜하지???
end
#cmd창 열어서 cd C:\Rselenium입력한다음 java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.11.0.jar -port 4445 입력 
install.packages('RSelenium')
library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = 'chrome')
remDr$open()
remDr$navigate("https://dhlottery.co.kr/store.do?method=topStoreRank&rank=1&pageGubun=L645")


# 본격적으로 크롤링
#페이지 숫자 찾아서 나머지도 호출해야함
lottoStore = c()

for(idx in 1:end) {                        ##루프출발
  front <- "selfSubmit("
  back  <- ")"
  script <- paste(front, idx, back, sep="")      ##for구문이해안됨..
  
  pagemove <- remDr$executeScript(script, args=1:2)
  
  # 소스 받아오기
  source <- remDr$getPageSource()[[1]]
  js_html <- read_html(source)
  
  js_link <- html_nodes(js_html,'tbody')
  stores <- js_link %>%
    html_nodes('tr') %>%
    html_nodes('td') %>%
    html_text()
  
  
  lottoStore = c(lottoStore, stores)
}                     ##루프 끝

lottoStore <- str_replace_all(lottoStore, "\t|\n|\r","")
lottoStore <- str_replace_all(lottoStore, "[[:space:]]","")

storeName <- NULL
cnt       <- NULL
address   <- NULL

for(idx in 1:length(lottoStore) ) {
  if(idx %% 5 == 2) {            
    storeName <- c(storeName, lottoStore[idx])
  }else if(idx %% 5 ==3) {
    cnt <- c(cnt, lottoStore[idx])
  }else if(idx %% 5 ==4) {
    address <- c(address, lottoStore[idx])
  }
}
lottoDF <- data.frame(storeName,cnt,address)
write.csv(lottoDF,"lotto_store.csv", row.names = F)  ##파일생성


#-------------------------------------------------------------- 서울시 쓰레기통 최적화 위치 찾기
trash <- read_excel(file.choose())
head(trash)
tail(trash)
str(trash)
select(trash,2)
trash <- trash[-c(1:2),]
binplace <- select(trash,(1:2))
names(binplace) <- c("번호","자치구")


ggplot(data = binplace, aes(x=자치구,group=자치구)) +
         geom_bar() +
         coord_flip()


population <- read.csv(file.choose(),header=T)   ##0인 값의 칼럼 빈칸으로 인식함

seoul <- select(population,'일자','연령대.10세단위.','성별','군구','유동인구수')
gu_population06 <- aggregate(seoul$유동인구수,list(seoul$군구),mean) ##구별 유동인구 평균
ggplot(data = gu_population06,aes(x=Group.1, y=x))+
  geom_bar(stat = 'identity')+
  coord_flip()

aggregate(seoul$유동인구수,list(seoul$성별),mean) #성별에 따른 월 평균 유동인구

age <- aggregate(seoul$유동인구수,list(seoul$연령대.10세단위.),mean) #나이에 따른 월 평균 유동인구 
ggplot(data = age, aes(x=Group.1, y=x))+
  geom_bar(stat='identity')

aggregate(seoul$유동인구수,list(seoul$일자),mean) #일자별 유동인구 평균

##자치구별 유동인구 / 쓰레기통 개수 변수를 담은 새로운 프레임워크 만들기
gu_population06$x <- round(gu_population06$x) #반올림처리
names(gu_population06) <- c("자치구","유동인구")
trashpopulation <- gu_population06  #새로운 프레임워크 생성
str(trashpopulation)

#자치구별 쓰레기통 개수확인
count(filter(binplace,자치구=='강남구'))

#변수생성
binpop<- trashpopulation %>%
  mutate(bin = ifelse(자치구 == "강남구",count(filter(binplace,자치구=='강남구')),
                         ifelse(자치구 == "강동구",count(filter(binplace,자치구=='강동구')),
                                   ifelse(자치구 == "강북구",count(filter(binplace,자치구=='강북구')),
                                             ifelse(자치구 == "강서구",count(filter(binplace,자치구=='강서구')),
                                                       ifelse(자치구 == "관악구",count(filter(binplace,자치구=='관악구')),
                                                                 ifelse(자치구 == "광진구",count(filter(binplace,자치구=='광진구')),
                                                                           ifelse(자치구 == "구로구",count(filter(binplace,자치구=='구로구')),
                                                                                     ifelse(자치구 == "금천구",count(filter(binplace,자치구=='금천구')),
                                                                                               ifelse(자치구 == "노원구",count(filter(binplace,자치구=='노원구')),
                                                                                                         ifelse(자치구 == "도봉구",count(filter(binplace,자치구=='도봉구')),
                                                                                                                   ifelse(자치구 == "동대문구",count(filter(binplace,자치구=='동대문구')),
                                                                                                                             ifelse(자치구 == "동작구",count(filter(binplace,자치구=='동작구')),
                                                                                                                                       ifelse(자치구 == "마포구",count(filter(binplace,자치구=='마포구')),
                                                                                                                                                 ifelse(자치구 == "서대문구",count(filter(binplace,자치구=='서대문구')),
                                                                                                                                                           ifelse(자치구 == "서초구",count(filter(binplace,자치구=='서초구')),
                                                                                                                                                                     ifelse(자치구 == "성동구",count(filter(binplace,자치구=='성동구')),
                                                                                                                                                                               ifelse(자치구 == "성북구",count(filter(binplace,자치구=='성북구')),
                                                                                                                                                                                         ifelse(자치구 == "송파구",count(filter(binplace,자치구=='송파구')),
                                                                                                                                                                                                   ifelse(자치구 == "양천구",count(filter(binplace,자치구=='양천구')),
                                                                                                                                                                                                             ifelse(자치구 == "영등포구",count(filter(binplace,자치구=='영등포구')),
                                                                                                                                                                                                                       ifelse(자치구 == "용산구",count(filter(binplace,자치구=='용산구')),
                                                                                                                                                                                                                                 ifelse(자치구 == "은평구",count(filter(binplace,자치구=='은평구')),
                                                                                                                                                                                                                                           ifelse(자치구 == "종로구",count(filter(binplace,자치구=='종로구')),
                                                                                                                                                                                                                                                     ifelse(자치구 == "중구",count(filter(binplace,자치구=='중구')),count(filter(binplace,자치구=='중랑구')))
                                                                                                                                                                                                                                                     ))))))))
                                                                                                                                                                     )))))))))))))))) %>%
  mutate(popperbin = 유동인구/unlist(bin))

binpop$popperbin <- round(binpop$popperbin) #반올림

#서울시 id코드 입력
binpop$id <- c("11680","11740","11305","11500","11620","11215","11530","11545","11350","11320","11230","11590","11440","11410","11650","11200","11290","11710","11470","11560","11170","11380","11110","11140","11260")

#상관분석
pairs.panels(air01)
cor(air01)
corrplot(air.cor , method = "ellipse",tl.col = "black", tl.srt = 0)


##시각화준비
install.packages('ggmap')
library(ggmap)
library(ggplot2)
install.packages('raster')
library(raster)
install.packages('rgeos')
library(rgeos)
install.packages('maptools')
library(maptools)
install.packages('rgdal')
library(rgdal)

#지도자료 받기/좌표계변환/기존자료와 병합하기
map <- shapefile("C:/TL_SCCO_SIG.shp")
map2 <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map2, region = 'SIG_CD')
new_map$id <- as.numeric(new_map$id)

seoul_map <- new_map[new_map$id <= 11740,] #서울시것만 따로 추출

seoul_merge <- merge(seoul_map,binpop,by='id') # 데이터병합

binpop$long <- c("127.06533399999999","127.149107","127.013272","126.824859","126.947435","127.088351","126.858351","126.902894","127.077134","127.034471","127.057221","126.953734","126.910326","126.94115499999999","127.03324499999999","127.043114","127.01969699999999","127.118003","126.85752599999999","126.91230299999999","126.981987","126.929119","126.97942","126.997985","127.095157")
binpop$lat <- c("37.493712000000002","37.547522000000001","37.640709999999999","37.558439999999997","37.464570000000002","37.543059","37.491580999999996","37.457774999999998","37.649734000000002","37.666330000000002","37.579132000000001","37.496074999999998","37.556708","37.574997000000003","37.470739999999999","37.54824","37.602916999999998","37.502167999999998","37.521940000000001","37.519739000000001","37.528582","37.616430999999999","37.592128000000002","37.557335000000002","37.595193999999999")
binpop$long <- as.numeric(binpop$long)
binpop$lat <- as.numeric(binpop$lat)     #위도경도 업데이트 및 숫자형변환

ggplot() + 
  geom_polygon(data = seoul_merge, aes(x=long, y=lat, group=group, fill = popperbin), color='white') +
  scale_fill_gradient(low = "#00CC99",
                      high = "#CC0000",
                      space = "Lab",
                      guide = "colourbar") +
labs(fill = "pop per bin") +
  geom_text(data = binpop,aes(x=long,y=lat,label=paste(자치구,popperbin,sep="\n")),size = 2.5)  #시각화 마무리 

mean(binpop$popperbin) # 평균적으로 1개당 138.92명을 커버하고 있음
(32619/138.92) -54 # 노원구가 평균적인 쓰레기통 개수를 보유하기 위해서는 180개가 더 필요하다 

#------------------------------------------------------
##----전체 맥주 데이터를 k-means로 군집분석 후  국내맥주와 비교
worldbeer <- read.csv(file.choose())
View(worldbeer)

#필요한 칼럼추출
x <- worldbeer[,c(9,10,11)]
str(x)
head(x)

y <- worldbeer[worldbeer$Style !="[0-9]",]
y <- y$Style
head(y)
str(y)

sum(is.na(x))
sum(is.na(y))

#K-means적용(10개는 임의로 지정)
kc <- kmeans(x,10)

table(y,kc$cluster)

plot(x[c("ABV","IBU","Color")],col = kc$cluster)
points(kc$centers[,c("ABV","IBU","Color")],col=1:10,pch=10,cex=2)