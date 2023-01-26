### 지난 1년간 카페에는 어떤 일이 있었을까? 

# install.packages("readxl")
library("readxl")

sales=read_xlsx("Cafe_Sales.xlsx",sheet=1)   # 2017년 9월 ~ 2020년 12월까지 데이터

head(sales) ; tail(sales)   # 데이터 파악

## 결측치
table(is.na(sales$order_id)) # 결측값 없음
table(is.na(sales$order_date)) # 결측값 171개 존재
sales=na.omit(sales) # 결측치 제거
nrow(sales) # 전체 행수

## 이상치
length(unique(sales$order_id)) # 여러 사람이 하나의 카드로 결제할 수 있음(주문 건수)
sort(unique(sales$order_id),decreasing = TRUE) # ID 내림차순
unique(sales$order_date)

# 범주형 자료 전체 항목
unique(sales$category)  # 종류
unique(sales$item)      # 이름
unique(sales$price)     # 가격

#----------------------------------------------------------------------------------------------------------#

## EDA
#item별 총판매금액
sort(table(sales$item),decreasing = TRUE)   # item별 판매량 내림차순

sales_tr=data.frame(table(sales$item))      # item별 판매량 data 생성

sales_item=subset.data.frame(sales,select = c("item","price")) # sales에서 item과 price 추출
sales_item=unique(sales_item) ; sales_item                     # item별 가격만 추출

item_list=merge(sales_tr,sales_item,by.x="Var1",by.y="item")    # tr과 item 데이터 메뉴기준 병합
item_list$amount=item_list$Freq*item_list$price                 # item별 총판매금액 계산


# 요일별 판매 메뉴
sales$weekday = weekdays(sales$order_date) # 날짜 -> 요일
table(sales$weekday) # 요일별 판매량 (주말에 판매량 높음)

date_info = data.frame(weekday=c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"),
                     day=c("평일","평일","평일","평일","평일","주말","주말"))
date_info

sales = merge(sales,date_info) ; head(sales)
table(sales$day) # 평일, 주말 판매량


#계절별 판매 메뉴
sales$month = months(sales$order_date) ; head(sales)  # month열 추가

attach(sales)   # attach 사용할 경우 sales$month 대신 month로 대체할 수 있다
for(i in 1:nrow(sales)){
  if(month[i]=="12월"| month[i]=="1월"| month[i]=="2월"){
    sales$season[i]="겨울"}
  else if(month[i]=="3월" | month[i]=="4월" | month[i]=="5월"){
    sales$season[i]="봄"}
  else if(month[i]=="6월" | month[i]=="7월" | month[i]=="8월"){
    sales$season[i]="여름"}
  else{
    sales$season[i]="가을"}
}
detach(sales)
head(sales)

table(sales$season)   # 계절별 판매량 (가을에 판매량이 가장 높음)

#----------------------------------------------------------------------------------------------------------#

## EDA (시각화)

# 카테고리별 판매 건수
library(ggplot2)    # install.packages("ggplot2")

target = data.frame(table(sales$category))

ggplot(target,aes(x=Var1,y=Freq))+
  geom_col()+
  geom_text(label=paste0(target$Freq,"건"),nudge_y=1000)  # 막대그래프


# 월별 판매 건수
sales$date_ym = format(sales$order_date,"%Y-%m")  # 날짜 형식 지정
target = data.frame(table(sales$date_ym))

target_12 = tail(target,12)   # 2020년 데이터만 추출
ggplot(target_12, aes(x=Var1, y=Freq, group=1)) +
  geom_line(size=1,color = "#000000",linetype=2) +
  geom_point(color = "#173F5F") +
  geom_text(aes(label = Freq), nudge_y=100)   # nudge는 text와 point 거리 지정


# 요일별 판매 건수
target = data.frame(table(sales$weekday))
target$Var1 = factor(target$Var1,levels=c("월요일","화요일","수요일",
                                        "목요일","금요일","토요일","일요일"))

target = target[order(target$Var1),]                # 오름차순 정렬
target$por = target$Freq / sum(target$Freq) * 100   # 요일별 판매량 비율

ggplot(target,aes(x="", y=por, fill=Var1)) +
  geom_col() +
  coord_polar(theta="y") +
  geom_text(aes(label=paste0(Var1,"\n",round(por,2),"%")), col = "white",
            position=position_stack(vjust=0.5)) +   
  # vjust 상하정렬, hjust 좌우정렬(0~1사이 값)
  scale_fill_manual(values=c("#000000","#222222","#444444",
                                      "#666666","#888888","#999999")) +
                                        theme(legend.position="none",
                                              panel.background=element_blank(),
                                              axis.text = element_blank(),
                                              axis.title = element_blank()) 