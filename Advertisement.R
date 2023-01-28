### 광고 정말 효과가 있을까?

library(readxl)   # install.packages("readxl")
adver=read_xlsx("abtest.xlsx",sheet=1)

## 결측치
str(adver$sales)                      # sales 변수 (문자형 + "NA" 존재)
adver=adver[adver$sales != "NA",]     # 결측값 제거
adver$sales=as.numeric(adver$sales)   # sales 숫자형 변환

##이상치
unique(adver$city1);unique(adver$city2)     # city1 : 시도 / city2 : 시군구
summary(adver$open);summary(adver$click)    # open : 이메일 오픈 횟수 / click : 쇼핑몰 열어본 횟수

# cf) type : A/B 테스트 집단 구분 / conversion : 구매 전환 수 / sales : 총 판매금액

#===========================================================================================================#

## EDA

# raster 패키지의 경우 지도 그리기 위해 필요
library(raster)     # install.packages("raster")
library(ggplot2)    # install.packages("ggplot2")

# GADM : 전 세계 행정구역 데이터베이스
korea=getData(name="GADM",country="kor",level=0)            # 국가
korea_sido=getData(name="GADM",country="kor",level=1)       # 시도
korea_sigungu=getData(name="GADM",country="kor",level=2)    # 시군구

p1=ggplot(korea)+
  geom_polygon(aes(x=long,y=lat,group=group),fill="white",color="black")+
  labs(title="Korea") +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

p2=ggplot(korea_sido)+
  geom_polygon(aes(x=long,y=lat,group=group),fill="white",color="black")+
  labs(title="Sido") +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

p3=ggplot(korea_sigungu)+
  geom_polygon(aes(x=long,y=lat,group=group),fill="white",color="black")+
  labs(title="Sigungu") +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

# p1, p2, p3 실행 시
p1    # 대한민국 지도
p2    # 시도
p3    # 시군구

#===========================================================================================================#

## A, B 그룹 분리
A_GROUP = subset.data.frame(x=adver,subset=c(type=="A_GROUP"))
B_GROUP = subset.data.frame(x=adver,subset=c(type=="B_GROUP"))

## 5000개 이상 정규성 검정시 
# H0 : 집단이 정규 분포를 따른다

library(nortest)  # install.packages("nortest")
ad.test(A_GROUP$open) ; ad.test(B_GROUP$open)       # 둘 다 정규성 만족 X (나머지 변수 동일)

## 정규성 만족할 경우 F-검정, 만족하지 않을 경우 Levene 검정
# H0 : 집단 간의 분산이 동일하다

library(car)      # install.packages("car")
leveneTest(y=adver$open,group=factor(adver$type))         # 등분산 만족
leveneTest(y=adver$click,group=factor(adver$type))        # 등분산 만족 X
leveneTest(y=adver$conversion,group=factor(adver$type))   # 등분산 만족 X

## t.test
# H0 : A와 B 집단의 광고 성과에 차이가 없다

t.test(A_GROUP$open,B_GROUP$open)                 # 통계적으로 유의 X
t.test(A_GROUP$click,B_GROUP$click)               # 통계적으로 유의 ( = 차이가 존재한다)
t.test(A_GROUP$conversion,B_GROUP$conversion)     # 통계적으로 유의

## 광고효과 지역별 파악
kyungki = subset.data.frame(x=adver,subset=c(city1=="경기도"))
kyungki_A = subset.data.frame(x=kyungki,subset=c(type=="A_GROUP"))
kyungki_B = subset.data.frame(x=kyungki,subset=c(type=="B_GROUP"))

shapiro.test(kyungki_A$open) ; shapiro.test(kyungki_B$open)   # 5000개 이하 데이터 정규성검정 (정규성 만족 X)
leveneTest(y=kyungki$open,group = factor(kyungki$type))         # 등분산 만족
wilcox.test(open~type,kyungki)                                # 정규성 만족 X -> 비모수 검정 (두 그룹간 차이 X)

# open에서는 차이가 없지만 모든 지역에 걸쳐서 click, conversion의 경우 차이가 존재

# 시각화
sido_key = data.frame(NAME_1=korea_sido$NAME_1,
                    KOR=c("부산광역시","충청북도","충청남도","대구광역시",
                          "대전광역시","강원도","광주광역시","경기도",
                          "경상북도","경상남도","인천광역시",
                          "제주특별자치도","전라북도","전라남도",
                          "세종특별자치시","서울특별시","울산광역시"),
                    RESULT = 1)   # RESULT = 0 : 효과가 없을 경우

korea_sido@data$id = rownames(korea_sido@data)
korea_sido@data = merge(korea_sido@data,sido_key,by="NAME_1")

koreaDf = fortify(korea_sido)
koreaDf = merge(koreaDf,korea_sido@data,by="id")

ggplot()+
  geom_polygon(data=koreaDf,aes(x=long,y=lat,group=group,fill=RESULT),color="black") +
  labs(title="지역별 두 그룹에 대한 성과 차이") +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

# 결과적으로는 모든 지역에서 광고의 효과가 존재