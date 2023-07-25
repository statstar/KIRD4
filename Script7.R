회귀분석 

단순회귀분석 y = f(x)

다중회귀분석 y = f(x1, x2, x3, x4, ..., x10)

#############################################
1. 산점도 plot(y~x)
2. 상관계수 확인
3. 회귀모형 세우기 lm(y~x)
4. summary 결과 해석하기. (회귀계수를 통한 선형식, 결정계수)
5. predict()를 이용한 새로운 예측
6. 자료의 시각화 

7. 정규성, 독립성, 등분산성 검토 -> 모형 개선
정규성(shapiro.test), 독립성(durbinWatsonTest), 등분산성(ncvTest)

8. 좋은 자료를 고르기

지렛대점 : 기존의 패턴을 잘 반영하지만 다른 자료와 많이 떨어진 자료
이상점 : 평균 직선으로 부터 많이 떨어진 지점 (잔차의 이상점)
영향점 : 특정 점이 있냐? 없냐?에 따라 절편과 기울기가 많이 변화는 점

lm(dist~speed, cars)
result <- lm(dist~speed, cars)

influencePlot(result)
# 지렛점 : 해당사항 없음
# 이상점 : 23, 49
# 영향점 : 49

hatvalues(result) %>% mean 

outlierTest(result)

result2 <- lm(dist~speed, cars[c(-23,-49),])

result %>% summary # 0.6511
result2 %>% summary # 0.7018

plot(dist~speed, cars)
points(dist~speed, cars[c(23,49),], pch=16)

abline(lm(dist~speed, cars), col="blue")
abline(lm(dist~speed, cars[c(-23,-49),]), col="red")

plot(result)

# 예제
plot(mpg~wt, data=mtcars)

fit <- lm(mpg~wt+I(wt^2), data=mtcars)
summary(fit)
# 정규성, 독립성, 등분산성
shapiro.test(fit$residuals) # 정규성 만족
durbinWatsonTest(fit)
ncvTest(fit) # 등분산성 만족

boxCox(fit)

fit1 <- lm(mpg^0.2~wt+I(wt^2), data=mtcars)
summary(fit1)
# 정규성, 독립성, 등분산성
shapiro.test(fit1$residuals) # 정규성 만족
durbinWatsonTest(fit1)
ncvTest(fit1) # 등분산성 만족

influencePlot(fit1) %>% rownames -> sel

mtcars %>% 
  filter(!rownames(.) %in% sel) -> mtcars1

fit2 <- lm(mpg^0.2~wt+I(wt^2), data=mtcars1)
summary(fit2)
# 정규성, 독립성, 등분산성
shapiro.test(fit2$residuals) # 정규성 만족
durbinWatsonTest(fit2) # 독립성
ncvTest(fit2) # 등분산성 만족

mpg ^ 0.2 = 2.234831 -0.163291*wt + 0.008307*wt^2
mpg = (2.234831 -0.163291*wt + 0.008307*wt^2)^5

fit0 <- lm(mpg~wt, data=mtcars1)
fit0 %>% summary

plot(mpg~wt, mtcars)
wt <- seq(1,5.2,0.1)
lines(wt, predict(fit0, data.frame(wt)), col="blue", lwd=2)
lines(wt, predict(fit2, data.frame(wt))^5, col="red", lwd=2)

## 영향점, 이상점, 지렛점을 찾을 수 있다.
influencePlot(fit1)

# StudRes : 이상점 (-2 < 정상값 < 2)
# Hat : 지렛대점 (제거하는게 좋음)
# CookD : 특정점이 있냐 없냐에 따라 모형이 많이 변함.

############################################################
library(quantreg)

plot(dist~speed, cars)
lm(dist~speed, cars) # 평균값
rq(dist~speed, data=cars) # 중앙값
abline(rq(dist~speed, tau=0.9, data=cars)) # 중앙값
abline(rq(dist~speed, tau=0.1, data=cars)) # 중앙값

#############################################################
# piecewise regression

x <- seq(1,10,0.5)
y <- ifelse(x<5,0.5*x+1, 3*x-10 )
e <- rnorm(length(x), sd=1)
y1= y+e
plot(y1~x)

library(segmented)
fit <- lm(y1~x)
seg.fit <- segmented(fit)
plot(seg.fit)
