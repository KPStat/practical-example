### KOSPI 예측이 가능할까?

library(quantmod)     # install.packages("quantmod")

# 2001년부터 현재까지 KOSPI 데이터 불러오기
KOSPI = getSymbols("^KS11",
                   from = "2001-01-01",
                   to = Sys.time(),
                   auto.assign = FALSE)     
# auto.assign은 데이터 불러오면 주가 정보를 회사 이름으로 할당

head(KOSPI) ; tail(KOSPI)

chartSeries(KOSPI)    # 주식 앱과 같은 이미지(시각화 자유도가 낮음)

#=================================================================================================#

## 시각화
str(KOSPI)    # stx 유형의 시계열 데이터

# 데이터 가공
sample = data.frame(date = time(KOSPI),
                    KOSPI,
                    growth = ifelse(Cl(KOSPI) > Op(KOSPI), "up","down"))
# 종가가 시가보다 높으면 up 아니면 down 으로 growth 열 생성

colnames(KOSPI)
colnames(sample) = c("date", "Open", "High", "Low", "Close", "Volume", "Adjusted", "growth")

head(sample)      # 변수명 보기 쉽게
summary(sample)   # 이상치나 큰 문제 없어 보임


# LOW(저가) 변수 이용
library(ggplot2)  # install.packages("ggplot2")
ggplot(sample, aes(x = date)) +
  geom_line(aes(y = Low))


# 2020년이후 데이터 이용해서 시각화
ggplot(sample[sample$date >= "2020-01-01",], aes(x = date)) +
  geom_linerange(aes(ymin = Low, ymax = High)) +      # 저가와 고가를 표현
  geom_rect(aes(xmin = date - 0.3,
                xmax = date + 0.3,
                ymin = pmin(Open, Close),
                ymax = pmax(Open, Close),
                fill = growth)) +                     # rect 이용해서 candle(봉) 표현
  guides(fill =  "none") +
  scale_fill_manual(values = c("down" = "blue", "up" = "red")) +
  labs(title = "KOSPI", subtitle = "2020-01-01 ~ ") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(hjust = 1),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "grey"),
        axis.ticks = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

#=================================================================================================================#

# KOSPI 종가 이용해서 시계열 분해

KOSPI_C = na.omit(KOSPI$KS11.Close)                 # 결측치 제거한 종가 데이터 불러오기
KOSPI_C = as.numeric(KOSPI_C)                       # 수치형으로 변환
ts_data = ts(data = KOSPI_C, frequency = 365)       # 시계열 자료로 변환

de_data_add = decompose(ts_data, type = "additive")           # 가법모형
de_data_multi = decompose(ts_data, type = "multiplicative")   # 승법모형

plot(de_data_add)
plot(de_data_multi)
# 분해 결과 관측, 추세, 계절성, 불규칙 요인을 시각화 해준다
# 다만 한계점이 존재하여 잘 쓰이지는 않음 (주가가 매해 반복되는 모양을 가지지는 않음)

#=================================================================================================================#

# forecast 패키지 이용 (2020-01-01 ~ 2021-01-31까지 데이터 사용)

KOSPI = getSymbols("^KS11",
                   from = "2020-01-01",
                   to = "2021-01-31",
                   auto.assign = FALSE)

ts_data = ts(data = as.numeric(KOSPI$KS11.Close), frequency = 5)

library(forecast)     # install.packages("forecast")


## trend(추세) 반영
fit_lm = tslm(ts_data ~ trend)    # 추세를 독립변수로 사용하여 시계열 회귀 모형 적합
fit_lm ; summary(fit_lm)


ggplot(KOSPI, aes(x = time(KOSPI), y = KS11.Close)) +
  geom_line() +
  geom_line(y = fit_lm$fitted.values, color = "grey")


## 월별 계절성 반영
ts_data = ts(data = as.numeric(KOSPI$KS11.Close), frequency = 12) 

fitted = tslm(ts_data ~ trend + season)
fitted ; summary(fitted)

ggplot(KOSPI, aes(x = time(KOSPI), y = KS11.Close)) +
  geom_line() +
  geom_line(y = fitted$fitted.values, color = "grey")


## 가변수 활용 (2020년 3월 코로나로 지수 급격히 떨어짐)
ts_data = ts(data = as.numeric(KOSPI$KS11.Close), frequency = 20) 
t = time(ts_data)

t.break = data.frame(t, ts_data)

t.break[t.break$t < 3.65,] = 0
t.break[t.break$t > 3.75,] = 0

tb1 = ts(t.break$t, frequency = 20)

fit.t = tslm(ts_data ~ t)
AIC(fit.t)

fit.tb = tslm(ts_data ~ t + I(t^2) + I(t^3) + I(tb1^3))   # 비선형모델 생성
AIC(fit.tb)   # AIC 감소


# 노란색 : 시계열 회귀 / 파란색 : 가변수 활용 시계열 회귀
ggplot(ts_data, aes(x = time(ts_data))) +
  geom_line(aes(y = ts_data)) +
  geom_line(aes(y = fit.t$fitted.values),
            color = "yellow", size = 1) +
  geom_line(aes(y = fit.tb$fitted.values),
            color = "blue")


new = data.frame(t= t[length(t)] + seq(1, by = 0.05, length.out = 20))    # forecast 이용하기 위해 데이터 유형 변경
forecast(fit.t, newdata = new)

#=================================================================================================================#

## auto.arima 이용

library(urca)     # install.packages("urca")

KOSPI = getSymbols("^KS11",
                   from = "2020-01-01",
                   to = "2021-01-31",
                   auto.assign = FALSE)

ts_kospi = ts(as.numeric(KOSPI$KS11.Close), frequency = 20)


ur_test = ur.kpss(ts_kospi)   # 단위근 검정(차분할지 결정)
summary(ur_test)              # 단위근 검정결과 3.2153으로 0.05보다 크기 때문에 차분을 진행

dif_1 = diff(ts_kospi, differences = 1)     # 차분 진행
ur_test2 = ur.kpss(dif_1) ; ur_test2        # 한번더 차분

dif_2 = diff(ts_kospi, differences = 2)
ur_test3 = ur.kpss(dif_2) ; ur_test3        # 유의수준 0.05보다 작으므로 차분 종료


library(forecast)   # install.packages("forecast")
fit = auto.arima(ts_kospi)    # 추천하지는 않음(실력이 늘지 않는다)
fit

# 잔차 검정
checkresiduals(fit)   # p-value가 0.05보다 크기 때문에 잔차간에 자기상관성 존재 X
                      # 그래프 상으로도 잔차가 White Noise를 만족하는 것처럼 판단된다

plot(forecast(fit, h = 20))   # 예측 시각화 (예측값, 80%와 95% 신뢰구간 표현)

# ARIMA 모형의 경우 차수(p,d,q)를 결정하는 것이 매우 중요함
# 스스로 모형을 추론해서 AIC와 BIC를 통해 모형을 판단할 수 있는 능력을 기르는 것이 중요