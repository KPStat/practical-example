sample = data.frame(id=c(NA,"id002","id003","id004","id005"),
                    math=c(89,70,NA,97,100))

is.na(sample)           # NA -> TRUE
table(is.na(sample))    # TRUE FALSE 개수 table 형태로 표현
table(is.na(sample$id)) # 특정 열만 확인 가능

mean(sample$math) ; min(sample$math) ; max(sample$math)   # NA 있을 경우 확인 불가
mean(sample$math, na.rm = TRUE)   # na.rm 옵션으로 NA 무시


# 결측치 제거
sample_omit1 = sample[!is.na(sample$id),]   # id 열 NA행 제거
sample_omit2 = sample_omit1[!is.na(sample_omit1$math),]   # math 열 NA행 제거

sample_omit3 = na.omit(sample) ; sample_omit3   # NA하나라도 있는 행 모두 제거


# 결측치 대체
math_avg = mean(sample$math, na.rm = TRUE)            # 결측값 제외 후 평균
sample[is.na(sample$math),2] = math_avg ; sample      # math 결측값 평균으로 대체

#--------------------------------------------------------------------------------------------#

# 이상치 처리
sample = data.frame(id = c("001","002","003","004","005","006"),
                    sex = c("M","F","F","C","M","F"),
                    math = c(89,70,85,97,100,120))

# 논리적 이상치 (우선적 제거)
table(sample$sex)   # 성별 빈도표(성별에 나올 수 없는 "C" 존재)
table(sample$math)  # 수학점수(120 존재)

sample_omit = sample[sample$sex != "C",]                # 성별 "C" 제거
sample_omit = sample_omit[sample_omit$math <= 100,]     # 수학점수 120 제거
sample_omit


# 이상치(극단값)
score = c(71,67,69,76,63,67,59,68,76,66,70,67,68,95,90,
          72,71,73,73,68,72,67,62,69,76,69,68,30,55,100)

boxplot(score)
Q1 = quantile(score, probs = 0.25) ; Q1     # 1사분위수
Q2 = quantile(score, probs = 0.5) ; Q2      # 2사분위수(중앙값)
Q3 = quantile(score, probs = 0.75) ; Q3     # 3사분위수

IQR = Q3 - Q1
LC = Q1 - 1.5 * IQR ; LC    # 상자그림 위 경계
UC = Q3 + 1.5 * IQR ; UC    # 상자그림 아래 경계

score_adj = score[score < UC & score > LC]  # 이상치 제거
score_adj

#--------------------------------------------------------------------------------------------#

## 피처 엔지니어링(Feature Engineering)
# 기존 변수 이용해서 새로운 정보 추가

score = read.csv("score.csv") ; score

score$total = score$math + score$kor    # 합계 생성
score$avg = score$total / 2             # 평균 생성
score

score$result = ifelse(score$avg > 80,"PASS","FAIL")   # 평균 80이상 PASS
score

table(score$result)             # 결과 테이블
table(score$sex,score$result)   # 성별에 따른 결과