# 푼거 날라가서 수업코드로 공부한거 올림

mydata = read.csv("Bank_customer_acquisition.csv")
str(mydata)
mydata$Loan=as.factor(mydata$Loan) #중요
str(mydata)

lpm.res = lm(Acquisition~.,mydata)
summary(lpm.res)
head(sort(predict(lpm.res)))
tail(sort(predict(lpm.res)))

logit.res = glm(Acquisition~., family=binomial(link=logit),data=mydata) 
#logit/probit모델에는 최소자승법 못쓰고 GLM이용
#family와 link 정의해줘야함
#family -> binomial (우리는 binary response model 다루는중)
#link -> logit하고 싶으면 logit, probit하고싶으면 probit
summary(logit.res)

yhat=rep(1,nrow(mydata)) #일단 모든값1인 벡터생성
predict(logit.res,type="response")
#type을 response로 설정하면 predicted funciton이 G(X베타) export함
mean(mydata$Acquisition)
predict(logit.res,type="response")<mean(mydata$Acquisition) #threshold와 비교

yhat[predict(logit.res,type="response")<mean(mydata$Acquisition)]=0 #True뜬 벡터들만 다 0으로 바꿈
#요거 이해하는게 매우 중요
yhat
confusion = table(yhat,mydata$Acquisition)
confusion
#464는 true negative
#116은 false negative  (actual은 1, predict은0  )
#220은 true postiive
#200 false positive 
sum(diag(confusion))/sum(confusion)
confusion[1,1]/sum(confusion[,1])
confusion[2,2]/sum(confusion[,2])

coef(logit.res) #베타값
colMeans(mydata[,1:4]) #x값 평균
summary(logit.res)

#probability of successful acquisition
exp(-2.474310+7.540167e-03*5.56570e+01+1.885287e-05*9.22310e+04+1.163095e-06*3.10222e+05+(-5.215652e-01)*1.64933e+00+2.394259e-02*1)/(1+exp(-2.474310+7.540167e-03*5.56570e+01+1.885287e-05*9.22310e+04+1.163095e-06*3.10222e+05+(-5.215652e-01)*1.64933e+00+2.394259e-02*1))
#마지막 dummy variable은 그냥 x값이 1(yes category)

xvalues = data.frame(Age=5.56570e+01 ,Income=9.22310e+04,HomeVal=3.10222e+05,Dist2Bank=1.64933e+00,Loan='Yes')
predict(logit.res,newdata=xvalues,type="response")

coef.lpm = coef(lpm.res)
coef.logit = coef(logit.res)
cbind(coef.lpm,coef.logit)

#partial effect 구하기
xvalues = data.frame(Age=5.56570e+01 ,Income=9.22310e+04,HomeVal=3.10222e+05,Dist2Bank=1.64933e+00,Loan='Yes')
xb = predict(logit.res,newdata=xvalues)# Xβ(선형예측값),linear combination value (response빼면)
xb
PE.logit = dlogis(xb) * coef(logit.res)[-1] #dlogis(Xb)는 G(X beta) 의미 (density(PDF))
PE.logit
#G(X beta)와 계수 곱하는거 => partial effects of all five X variables

PE.lpm =coef(lpm.res)[-1] #lpm에서는 그냥 계수 자체가 partial effect 의미
cbind(PE.lpm, PE.logit)
#여기서 partial effect가 의미하는건, 하나 늘어날때마다 offer 받을 확률이 그만큼 올라간다는것
#마지막 Loanyes는 이미 loan있는(1값)사람들이 0.513%만큼 offer 받을 확률 높다는것

