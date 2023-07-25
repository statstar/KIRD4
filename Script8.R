 Y = f(X) + e
 f(X) = a + bx : 일차선형 관계
##########################################
 lm(Y~X)/ 정규성, 독립성, 등분산성
 influenceplot(모형) 이상점, 지렛대, 영향점
 
단순회귀모형 -> 다중회귀모형 (독립변수가 여러개 확장)

다중회귀모형
 - 다중공선성 : vif(분산팽창지수)>5 
   변수선택, lasso, ridge, elastic net

 - 변수선택 (모형선택)
   전진선택
   후진제거 ***
   단계적선택 : 전진선택 -> 후진제거 -> 전진선택 -> 후진제거 

## 다중공선성
  독립변수 간 상관성이 커서 추정된 회귀계수의 분산을 크게 추정하는 문제

# vif(분산팽창지수) : 10이상이면 다중공선성 (5이상)
head(airquality)
model0 <- lm(Ozone ~ Solar.R + Wind + Temp + Month, data=airquality)
summary(model0)
library(car)
library(dplyr)
vif(model0) # 다중공선성 확인
cor(airquality %>% filter(complete.cases(.)))

# 다중공선성의 문제가 있다면 해결방법은?
## 1. 주성분 회귀분석 (X)
## 2. 변수선택 (쉬운 방법)
## 3. 정규화 회귀모형 
  LASSO regression, Ridge regression, Elastic Net regression

library(glmnet)
  
library(MASS)
data(longley)  
fit.ml <- lm(Employed~.-Year, data=longley)
summary(fit.ml)  
vif(fit.ml)  

# 변수선택
fit.ml <- lm(Employed~.-Year-GNP-Population, data=longley)
fit.ml %>% vif

library(glmnet)
x <- longley %>% 
  select(-Employed, -Year) 
y <- longley %>% 
  select(Employed) 

lasso_model <- glmnet(as.matrix(x), as.matrix(y), alpha=1) #학습시키기
plot(lasso_model, xvar="lambda") #람다에 따른 회귀계수 확인하기

ridge_model <- glmnet(as.matrix(x), as.matrix(y), alpha=0) #학습시키기
plot(ridge_model, xvar="lambda") #람다에 따른 회귀계수 확인하기


- 변수선택 (모형선택)
 전진선택법
 후진제거법
 단계적 변수선택법
 
library(leaps)
fit.subset <- regsubsets(Employed~.-Year, data=longley)
# 수정된 결정계수 : 결정계수는 모형의 설명력 - 패널티(변수의 수)

plot(fit.subset, scale="adjr2") # GNP, Unemployed, Armed.Forces, Population
plot(fit.subset, scale="bic") # GNP, Unemployed, Armed.Forces
plot(fit.subset, scale="Cp") # GNP, Unemployed, Armed.Forces

# 어떤 모형을 선택해야하나요???

해결방안 

train(70%) # 훈련용 자료
test(30%)  # 검증용 자료

모형은 : train(70%) 회귀모형 
 model 1 : GNP, Unemployed, Armed.Forces, Population
 model 2 : GNP, Unemployed, Armed.Forces
 
predict(model1, test) : 예측

sqrt((test$Employed - predict(model1, test))^2 /n) : RMSE
sqrt((test$Employed - predict(model2, test))^2 /n) : RMSE
