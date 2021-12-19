#전체 데이터 읽기 
data = read.csv("regressionAnalysis2Homework/국민건강보단_건강검진정보_20201231.csv", fileEncoding="CP949")
head(data)


#전체 데이터 중 필요한 데이터 골라오기
sortedData = data[, c('성별코드', '연령대.코드.5세단위.','수축기.혈압', '체중.5Kg.단위.', '음주여부', '흡연상태')]
head(sortedData)


# 음주여부 0 데이터가 NA처리 돼있기 때문에 0으로 바꿔줌 
table(sortedData$음주여부)
sortedData$음주여부[is.na(sortedData$음주여부)] = 0
table(sortedData$음주여부)
# 이건 만약에 1,2로 바꿀꺼면 추가해주기 sortedData$음주여부 = ifelse(sortedData$음주여부==1,2,1)


#회귀분석
fit = lm(수축기.혈압 ~ factor(성별코드) + 연령대.코드.5세단위. + 체중.5Kg.단위. + factor(흡연상태) + factor(음주여부), sortedData)
summary(fit)


# 다중공선성 여부 판명
library(car)
vif(fit)
