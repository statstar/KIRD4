# 연속형 * 연속형

head(cars)

# 단순회귀모형
plot(dist~ speed, cars, pch=16) # 시각적으로 확인
cor(cars$dist, cars$speed) # 상관계수 

result <- lm(dist~ speed, cars)
summary(result)

# Residuals 
cars$dist - result$fitted.values # 관측값 - 예측값
(cars$dist - result$fitted.values) %>% summary
## 잔차가 정규분포 검토

# Coefficient
dist= 절편 + 기울기 * speed
dist = -17.5791 + 3.9324 * speed
##귀무가설 : 기울기가 0이다.

# 모형요약
R^2 : 결정계수 : 65.1%

cars$dist 분산 : sum(자료 - 평균)^2 : 변동제곱합
sum(y - 예측값 + 예측값 - y 평균)^2
 = sum(y- 예측값)^2 + sum(예측값 - y 평균)^2
결정계수 = 1 - sum(y- 예측값)^2 /  변동제곱합 

# 새로운 값 예측
predict(result, data.frame(speed=c(23,24,32,35)),
        interval="confidence") # 평균 예측값의 95%
predict(result, newdata=data.frame(speed=23),
        interval="prediction") # 새로운 객체의 예측값의 95%

# 그림 
library(ggplot2)
ggplot(cars, aes(x=speed, y=dist))+
  geom_point()+
  geom_smooth(method="lm")+theme_classic()

lm(dist~speed-1, cars)

alligator = data.frame(
  lnLength = c(3.87, 3.61, 4.33, 3.43, 3.81, 3.83, 3.46, 3.76,
               3.50, 3.58, 4.19, 3.78, 3.71, 3.73, 3.78),
  lnWeight = c(4.87, 3.93, 6.46, 3.33, 4.38, 4.70, 3.50, 4.50,
               3.58, 3.64, 5.90, 4.43, 4.38, 4.42, 4.25)
)

plot(lnWeight~lnLength, alligator)
cor(alligator$lnWeight, alligator$lnLength)^2

result <- lm(lnWeight~lnLength, alligator)
summary(result)

predict(result, data.frame(lnLength=4.5),
        interval="prediction")

ggplot(alligator, aes(x=lnLength, y=lnWeight))+
  geom_point()+
  geom_smooth(method="lm")+theme_classic()
