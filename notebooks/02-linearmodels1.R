# linearmodels_간단서머리추출

data = read.csv("usedcars.csv",header=T)
str(data)
data <- data[,-c(1,2)]
str(data)
lm.res = lm(Price~.,data)
summary(lm.res)
yhat = fitted(lm.res) #예측
yhat
resid(lm.res)


mydata = read.csv("advertising.csv",header=T)
head(mydata)
attach(mydata) #이거가지고 작업하겠다 설정
lm.res = lm(sales~TV+radio+newspaper,mydata) #linear model
lm.res
names(lm.res)
lm.res$coefficients
lm.sum=summary(lm.res)
lm.sum
names(lm.sum)
lm.sum$coefficients

yhat = fitted(lm.res) #예측
yhat
uhat = resid(lm.res) #잔차 예측
uhat
cbind(TV, radio, newspaper, sales, yhat, uhat)[1:10,]

bhat = lm.sum$coefficients[,1]
se = lm.sum$coefficients[,2]
tstat = bhat /se #tstat 수기확인
tstat
cbind(tstat,lm.sum$coefficients[,3]) # summary상 tstat과 일치하는거 확인

df = lm.res$df.residual
alpha = 1-0.95
qt(1-alpha/2,df) #q function of t distribution, ttest의 cutoff 구하기 ->percentage&자유도 넣음

qnorm(1-alpha/2)

