#linear model_predictive

data = read.csv("usedcars2.csv",header=T)
str(data)
data <-data [,c(-1,-2,-7,-9,-10)]
data$Color <- as.factor(data$Color)
str(data)
data[]
lm.res=lm(Price~.,data)
lm.sum=summary(lm.res)
lm.sum

data <-data [,c(-8)]
str(data)

# Color 제외, Age와 KM의 interaction 포함
lm.int <- lm(Price ~ Age * KM + HP + Automatic + Gears + Weight, data = data)
str(data)
# 요약 출력
summary(lm.int)

# 1) 산점도: KM vs Price
plot(data$KM, data$Price,
     pch = 16, cex = 0.6,
     xlab = "KM (kilometers driven)",
     ylab = "Price (€)",
     main = "Scatterplot: KM vs Price")

# (눈으로 보면 KM이 늘수록 Price가 떨어지는데,
#   완전 직선이라기보단 초반 급락 후 완만—즉, 비선형 패턴이 보입니다.)

# 2) 다항회귀(Polynomial of KM up to degree 4) + Automatic
#    raw=TRUE: 표준 다항식 기반(그냥 KM, KM^2, KM^3, KM^4)
#    Automatic이 0/1이면 numeric으로 둬도 되고, factor여도 자동으로 처리됩니다.
lm3 <- lm(Price ~ poly(KM, 4, raw = TRUE) + Automatic, data = data)
summary(lm3)

# 3) 총 계수(파라미터) 개수 확인
#    Intercept + KM(4개 항) + Automatic(1개) = 보통 6개
length(coef(lm3))
coef(lm3)

# 4) 예측 곡선 그리기: Automatic은 데이터의 평균(= 0/1이면 '자동변속기 비율')로 고정
km.grid <- seq(from = min(data$KM, na.rm = TRUE),
               to   = max(data$KM, na.rm = TRUE),
               by   = 1000)

auto_mean <- mean(data$Automatic, na.rm = TRUE)  # 0/1이면 비율

preds <- predict(
  lm3,
  newdata = list(KM = km.grid, Automatic = rep(auto_mean, length(km.grid)))
)

# 5) 산점도 위에 예측 곡선 추가

lines(km.grid, preds, lwd = 3)
