#censored data_tobit모델

#task 1
library(censReg)
data = read.csv("Mobile_data_usage.csv",header=T)
x=data$Quota
y=data$DataUse
plot(x,y)
sort(y,decreasing=FALSE)

lpm.res=lm(DataUse~Quota+Days,data)
summary(lpm.res)

tobit.res=censReg(DataUse~Quota+Days,left=0,data=data)
summary(tobit.res)

coef(tobit.res)[1:3]
beta = coef(tobit.res)[1:3]
sigma= exp(coef(tobit.res)[4])
sigma #실제 sigma는 1(standard norm distribution은 mean=0, sigma=1)

xb=beta[1]+beta[2]*x+beta[3]*mean(data$Days)

lines(x, xb, lty=1) #censored는 tobit모델이 해결

#Partial Effects
Ey = pnorm(xb/sigma)*xb + sigma*dnorm(xb/sigma) #pnorm은 표준정규분포의 CDF, dnorm은 PDF
lines(x,Ey,lty=1) #expected value of y

marg.eff = pnorm(xb/sigma)*beta[2]
lines(x,marg.eff,lty=1)
#x가 작은 부분은 거의 marginal 효과가 0(y가 음수일가능성이 매우 높으므로)
#x가 큰 부분들은 y가 양수일 가능성이 매우 높으므로 marginal effect가 진짜식의 기울기인 1으로 수렴함

abline(h=coef(lm.res)[2],lty=2) #그냥 선형회귀는 marginaleffect를 그냥 평균내서 constant 0.5로감

margEff(tobit.res,c(1,10,1)) #1은 y절편(intercept), 2.5는 x값 => x값 작을떄는 0에 가깝고
margEff(tobit.res,c(1,2000,1)) #x값 클때는 1에 가까워짐

