library(dplyr)
library(plm)
library(stargazer)
library(ggplot2)
library(pglm)
library(maxLik)

# 데이터 파일 불러오기
##노트북 용
#klips_raw <- read.csv("C:/Users/USER/Desktop/24_1/ecn4003/Final_project/0613/0613_test_240613/0613_test_240613.csv")
##데스크탑 용
klips_raw <- read.csv("C:/Users/wjddn/Desktop/ECN4003/Econometrics_Final-main/0614_2_240614.csv")



# 종속 변수 = 결혼 경험 유무
klips<- klips_raw %>% filter(!is.na(p_married))
klips <- klips %>%
  mutate(marry = ifelse(p_married == 1, 0, 1))



# 독립 변수 시작 -------------------------------------------------------------
#일자리 안정성 ---------------------------------------------------------------
###고용 안정성
klips <- klips[!is.na(klips$p0601), ]
klips <- klips %>%
  mutate(S_stable_employment = ifelse(p0601 == 1, 1, 0))

###불규칙적 일자리
klips <- klips[!is.na(klips$p0508), ]
klips <- klips %>%
  mutate(S_irregular_work = ifelse(p0508 == 1, 1, 0))

###근로시간의 규칙성
klips <- klips[!is.na(klips$p1002), ]
klips <- klips %>%
  mutate(S_stable_work_time = ifelse(p1002 == 1, 1, 0))

### 초과근로여부
klips <- klips[!is.na(klips$p1011), ]
klips <- klips %>%
  mutate(S_overtime_work = ifelse(p1011 == 2, 1, 0))


#금전적인 부분 ----------------------------------------------------------------
###연간소득
##cpi 만들기
klips <- klips%>%
  mutate(cpi = ifelse(year == 2013, 93, 100))
klips <- klips%>%
  mutate(cpi = ifelse(year == 2014, 94.2, cpi))
klips <- klips%>%
  mutate(cpi = ifelse(year == 2015, 94.9, cpi))
klips <- klips%>%
  mutate(cpi = ifelse(year == 2016, 95.8, cpi))
klips <- klips%>%
  mutate(cpi = ifelse(year == 2017, 97.6, cpi))
klips <- klips%>%
  mutate(cpi = ifelse(year == 2018, 99.1, cpi))
klips <- klips%>%
  mutate(cpi = ifelse(year == 2019, 99.5, cpi))
klips <- klips%>%
  mutate(cpi = ifelse(year == 2021, 102.5, cpi))
klips <- klips%>%
  mutate(cpi = ifelse(year == 2022, 107.7, cpi))
# 모름이라고 답한 인원들을 NA처리 후 제거
klips <- klips %>%
  mutate(annual_income = ifelse(p_wage <= 0, NA, p_wage))
klips <- klips %>% filter(!is.na(annual_income))
# 연간소득 하위 1%와 상위 1% 임계값 계산
lower_bound <- quantile(klips$annual_income, 0.01)
upper_bound <- quantile(klips$annual_income, 0.99)
# 하위 1% 필터링
klips <- klips %>%
  filter(annual_income >= lower_bound)

#실질임금
klips <- klips %>%
  mutate(real_income = annual_income / (cpi / 100))

#로그
klips <- klips %>%
  mutate(M_log_income = log(real_income))




###성과급제여부
# 모름이라고 답한 인원들을 NA처리 후 제거
klips <- klips %>%
  mutate(p1621 = ifelse(p1621 == 3, NA, p1621))
klips <- klips %>% filter(!is.na(p1621))
# p1621 값을 1인 사람 외에는 0으로 변환
klips <- klips %>%
  mutate(M_Incentive_Program = ifelse(p1621 == 1, 1, 0))



#사내 복지적인 부분------------------------------------------------------------
###휴가 _ 출산 휴가
# 모름이라고 답한 인원들을 NA처리 후 제거
klips <- klips %>%
  mutate(W_Baby_leave = ifelse(p4109 <= 0, NA, p4109))
klips <- klips %>%
  mutate(W_Baby_leave = ifelse(p4109 == 3, NA, W_Baby_leave))
klips <- klips %>% filter(!is.na(W_Baby_leave))
klips <- klips %>%
  mutate(W_Baby_leave = ifelse(W_Baby_leave == 1, 1, 0))

###휴가 _ 육아 휴직
# 모름이라고 답한 인원들을 NA처리 후 제거
klips <- klips %>%
  mutate(W_Parental_leave = ifelse(p4113 <= 0, NA, p4113))
klips <- klips %>%
  mutate(W_Parental_leave = ifelse(p4113 == 3, NA, W_Parental_leave))
klips <- klips %>% filter(!is.na(W_Parental_leave))
klips <- klips %>%
  mutate(W_Parental_leave = ifelse(W_Parental_leave == 1, 1, 0))

###휴가 _ 가족돌봄 휴직
# 모름이라고 답한 인원들을 NA처리 후 제거
# klips <- klips %>%
#   mutate(Family_Care_Leave = ifelse(p4173 <= 0, NA, p4173))
# klips <- klips %>% filter(!is.na(Family_Care_Leave))
# klips <- klips %>%
#   mutate(Family_Care_Leave = ifelse(Family_Care_Leave == 1, 1, 0))

###금전지원 _ 주택마련 지원
# 모름이라고 답한 인원들을 NA처리 후 제거
klips <- klips %>%
  mutate(W_Housing_support = ifelse(p4135 <= 0, NA, p4135))
klips <- klips %>%
  mutate(W_Housing_support = ifelse(p4135 == 3, NA, W_Housing_support))
klips <- klips %>% filter(!is.na(W_Housing_support))
klips <- klips %>%
  mutate(W_Housing_support = ifelse(W_Housing_support == 1, 1, 0))

###금전지원 _ 보육비 지원
klips <- klips %>%
  mutate(W_Child_care_support = ifelse(p4143 <= 0, NA, p4143))
klips <- klips %>%
  mutate(W_Child_care_support = ifelse(p4143 == 3, NA, W_Child_care_support))
klips <- klips %>% filter(!is.na(W_Child_care_support))
klips <- klips %>%
  mutate(W_Child_care_support = ifelse(W_Child_care_support == 1, 1, 0))




# ---------------------------------------------------------------------------- #

# 패널 로짓 분석 수행
# 패널 데이터 설정
pdata <- pdata.frame(klips, index = c("pid", "year"))

# 로짓 패널 모델 설정
model <- plm(marry ~ S_overtime_work + S_stable_work_time + S_stable_employment + S_irregular_work + 
               M_log_income + M_Incentive_Program  +
               W_Baby_leave + W_Housing_support + W_Parental_leave + W_Child_care_support,
             data = pdata,
             model = "within",
             effect = "individual",
             family = binomial)
# 결과 요약 출력
summary(model)



