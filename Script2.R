library(readxl)
birth <- read_excel("C:/Users/stats/Downloads/KIRD3-main/KIRD3-main/anova_one_way.xlsx")

head(birth)
# 시각적 확인
boxplot(birth_rate~ ad_layer, birth)

birth %>% 
  group_by(ad_layer) %>% 
  summarise(n=n(), mean=mean(birth_rate), sd=sd(birth_rate))

# 일원배치분산분석
정규성, 독립성, 등분산성

bartlett.test(birth_rate~ ad_layer, birth)
# p<0.05 등분산성 만족하지 않음

oneway.test(birth_rate~ ad_layer, birth)

out <- aov(birth_rate~ ad_layer, birth) #등분산성 가정

TukeyHSD(out) %>% plot

# 범주형 * 범주형 : chisq.test
# 범주형 * 연속형 : t.test, aov, oneway.test
# 연속형 * 연속형 : cor.test, lm 




