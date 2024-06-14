library(dplyr)
library(plm)
library(stargazer)


# 데이터 파일 불러오기
klips_raw <- read.csv("C:/Users/USER/Desktop/24_1/ecn4003/Final_project/0613/0613_test_240613/0613_test_240613.csv")


# 결혼 경험 유무
klips<- klips_raw %>% filter(!is.na(p_married))
klips <- klips %>%
  mutate(marry = ifelse(p_married == 1, 0, 1))


#일자리 안정성
###불규칙한 일자리
klips <- klips[!is.na(klips$p0508), ]
klips <- klips %>%
  mutate(irregular_work = ifelse(p0508 == 1, 1, 0))

###근로시간의 규칙성
klips <- klips[!is.na(klips$p1002), ]
klips <- klips %>%
  mutate(irregular_work = ifelse(p1002 == 1, 1, 0))


#금전적인 부분
###세전연간소득

###성과급제여부