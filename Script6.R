# 독립성을 따르지 않은 경우 예
x = 1:10
y = 50 - 30*x +4*x^2
e = rnorm(10, sd=10)
y2 <- y + e

plot(y2~x)
abline(lm(y2~x), col="blue")
out <- lm(y2~x) 
out1 <- lm(y2~x+I(x^2)) 

# 잔차도표
plot(out$fitted.values, out$residuals)
durbinWatsonTest(out)

plot(out1$fitted.values, out1$residuals)
durbinWatsonTest(out1)
