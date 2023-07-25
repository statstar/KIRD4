회귀분석

연속형 * 연속형 : 인과관계

head(airquality)
plot(sqrt(Ozone) ~Temp, airquality) # 일차선형 관계가 아니면 
result <- lm(sqrt(Ozone) ~Temp, airquality)
result %>% summary

predict(result, data.frame(Temp = 100),
        interval="prediction")
predict(result, data.frame(Temp = 100),
        interval="confidence")

y = f(x) + e,  e ~ i.i.d N(0, sigma^2)

# 좋은 회귀모형 : 오차항이 동질하고 독립적인 정규분포 
정규성, 독립성, 등분산성

result = lm(dist~speed, cars)
summary(result)

# 정규성 
## 시각적
qqnorm(result$residuals)
qqline(result$residuals)

shapiro.test(result$residuals)
# p<0.05, 정규분포를 만족하지 않음

library(car)
boxCox(result)

result1 = lm(dist^0.5~speed, cars)
shapiro.test(result1$residuals)

## 독립성
### 시각적 : 잔차도표
plot(result1$fitted.values, result1$residuals)

### 정량적 
durbinWatsonTest(result1)
# p>0.05 서로 독립적이다. 

## 등분산성
spreadLevelPlot(result1) ## 시각적
ncvTest(result1)

result %>% summary
result1 %>% summary

result1 = lm(dist^0.5~speed, cars)

dist^0.5 = 1.2771 + 0.3224*speed

dist = (1.2771 + 0.3224*speed)^2

plot(dist~speed, cars)
speed <- seq(5,25,1)
predict(result1, newdata=data.frame(speed))^(1/0.5)

plot(dist~speed, cars)
lines(speed, predict(result1, newdata=data.frame(speed))^(1/0.5), col="blue")

##############################
회귀모형 세우고 난 뒤 
정규성, 독립성, 등분산성 체크해봐야 한다.

library(car)

정규성 : shapiro.test(result$residuals)
독립성 : durbinWatsonTest(result)
등분산성 : ncvTest(result)

정규성 만족하지 않으면 boxCox(result)
독립성을 만족하지 않으면 : 1차선형모형이 아닌 다른 모형
등분산성 spreadLevelPlot(result) # suggested power를 y의 승수로

# alligator 회귀모형이 정규성, 독립성, 등분산성을 만족하는지 검토
result <- lm(lnWeight~lnLength, alligator)
summary(result)

shapiro.test(result$residuals) # 정규성 X
durbinWatsonTest(result) # 독립성 O
ncvTest(result) # 등분산성 O

boxCox(result)

result1 <- lm(lnWeight^0.6~lnLength, alligator)
summary(result1)

shapiro.test(result1$residuals) # 정규성 O
durbinWatsonTest(result1) # 독립성 O
ncvTest(result1) # 등분산성 X

plot(result1)
