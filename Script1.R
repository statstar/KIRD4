강의자료 : github.com/statstar

# 1. R의 기본원리
1. <-, =, -> 객체를 저장해서.
2. 함수() : 연구자가 원하는 결과가 도출되는 알고리즘
3. 패키지 
4. [], index (원하는 자료만 선택)

# 2. 데이터 분석과정
Import -> Tidy -> Model -> Communicate

# Import (자료 확인)
head, tail, str, summary, dim, View 

# Tidy(데이터를 분석에 맞도록 수정)
Filter : 논리식을 이용한 행자료 선택
Select : 숫자, 변수 이름을 이용하여 열 자료 선택(변수)
Mutate : 새로운 변수 만들기
Arrange : 자료를 정렬
Group_by : 집단별 결과를 생성
Summarise : 요약값 생성

# Model (통계분석, 데이터분석)
library(dplyr)
library(ggplot2)
head(mpg)

mpg$hwy # 연속형 자료
mpg$hwy %>% summary
mpg$hwy %>% hist
mpg$hwy %>% density %>% plot
mpg$hwy %>% boxplot
mpg$hwy %>% shapiro.test() # 정규성 검정

mpg$hwy %>% mean
t.test(mpg$hwy, mu=22.0)
 # p-value <0.05, 귀무가설 기각, 평균연비가 22이라 말할 수 없다.

mpg$drv %>% table

mpg %>% 
  filter(drv != "r") -> mpg1

mpg1$drv %>% table

# 그림으로 시각화
boxplot(hwy ~ drv, mpg1)
library(vioplot)
vioplot(hwy ~ drv, mpg1)

# 요약값 
mpg1 %>% 
  group_by(drv) %>% 
  summarise(n=n(), mean=mean(hwy), sd=sd(hwy))

# 검정통계량, 유의확률
var.test(hwy ~ drv, mpg1) # 분산의 동질성
# p=0.754 >0.05 귀무가설 채택, 분산이 같다.

t.test(hwy ~ drv, mpg1, var.equal=T)

# 대응 t 검정
boxplot(mpg$cty, mpg$hwy)
t.test(mpg$cty, mpg$hwy, paired=T)

t.test(mpg$cty, mpg$hwy, paired=T)

# 카이제곱 검정

범주형 * 범주형 

table(mpg$class, mpg$drv) %>% t %>% 
  barplot(beside=T, legend=rownames(.))

table(mpg$class, mpg$drv) %>% chisq.test

plot(dist~speed, cars)
연속형 * 연속형 
cor.test(cars$dist, cars$speed)

0.8 < r < 1.0 : 매우 높은 상관성이 있다.
0.6 < r < 0.8 : 높은 상관성이 있다.
0.4 < r < 0.6 : 상관성이 있다.
0.2 < r < 0.4 : 낮은 상관성이 있다. 
0.0 < r < 0.2 : 매우 낮은 상관성이 있다. 

speed(원인) -> dist(결과) : 인과관계
lm(dist~ speed, cars)
abline(lm(dist~ speed, cars), col="blue")
dist = f(speed) + e : f()함수가 일차선형식 y = ax + b

