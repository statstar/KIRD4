# 회귀분석 
  
  종속변수 = f(독립변수) + e(오차항), e ~ i.i.d 정규분포 가정
  
  오차항의 가정 : 독립성, 정규성, 등분산성 체크 필요
  
  f(독립변수) = a + b * 독립변수 : 선형회귀모형
                  a + s(독립변수)  : 일반화가법모형
                  
  lm(종속변수 ~ 독립변수, data=데이터명)
  독립변수 : 연속형 
  독립변수 : 범주형(문자형), factor(숫치형)
  
  lm(Ozone~Month, airquality)
  lm(Ozone~factor(Month), airquality)
  lm(Ozone~Month -1, airquality) # 원점을 지나가는 직선
  lm(Ozone~Month +0, airquality) # 원점을 지나가는 직선
  lm(Ozone~Month+I(Month^2), airquality)
  lm(Ozone~Month+I(Month^2)+I(Month^3), airquality)

  summary # 회귀계수, 회귀계수 유의성, 결정계수
  predict(모형, newdata=data.frame(독립변수))
  
  오차의 가정 : 정규성(sharpiro.test), 독립성(durbinWatsonTest), 등분산성(ncvTest)
  
  좋은 데이터 : influencePlot(모형) / 지렛대점, 영향점, 이상점 
  
  나쁜 데이터 제거 후 다시 회귀모형 세우기
#################################################################################
다중회귀모형 
  - 다중공선성 : vif>5, lasso model, ridge model : glmnet
  - 변수선택: 전진선택, 후진제거, 단계적변수선택 
              regsubsets()
              수정된결정계수 : 크면 클수록 좋음
              BIC, AIC : 낮으면 낮을수록 좋은
              Cp : 낮을 수록 좋음.(p)
              
###########################################################################
# 회귀모형의 일반화
              
# 현재 : Big data (Volume이 크다)
# 자료를 다 이용할 필요가 없다.
# 데이터를 일부를 훈련용, 남은 자료를 검증용
             
states <- as.data.frame(state.x77[,c("Murder", "Population",
                       "Illiteracy", "Income", "Frost")])
head(states)
fit=lm(Murder~Population+Illiteracy+Income+Frost,data=states)
summary(fit)              

set.seed(23434)  
sel <- sample(1:nrow(states), 40)
  
train.d <- states[sel,]
test.d <-  states[-sel,]

dim(train.d)
dim(test.d)

sub.m <- regsubsets(Murder~Population+Illiteracy+Income+Frost,data=train.d)
plot(sub.m, scale="adjr2") # Population, Illiteracy
plot(sub.m, scale="bic") # Population, Illiteracy
plot(sub.m, scale="Cp") # Population, Illiteracy

m1 <- lm(Murder~Population+Illiteracy+Income+Frost,data=train.d)
m2 <- lm(Murder~Population+Illiteracy,data=train.d)

vif(m1)

predict(m1, test) # 예측값
predict(m2, test) # 예측값

(test$Murder - predict(m1, test))^2 %>% sum
(test$Murder - predict(m2, test))^2 %>% sum

m1 %>% summary # 0.4569
m2 %>% summary # 0.4566

library(randomForest)
lm.fit <- lm(Murder~Population+Illiteracy+Income+Frost,data=train.d)
rf.fit <- randomForest(Murder~Population+Illiteracy+Income+Frost,data=train.d)

(test$Murder - predict(lm.fit, test))^2 %>% sum
(test$Murder - predict(rf.fit, test))^2 %>% sum
# 예측정확도는 높지만, 해석이 어려움 

rf.fit %>% varImpPlot()

# 상대적 중요도 : 표준화계수
scale.train <- scale(train.d) # 평균이 0, 분산이 1

scaled.lm <- lm(Murder~Population+Illiteracy+Income+Frost,
                data=scale.train %>% data.frame)
round(scaled.lm$coefficients ,3)