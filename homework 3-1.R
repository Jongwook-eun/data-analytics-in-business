mydata =read.csv("Forum_Posts.csv")

library(MASS)
str(mydata)

poisson.res=glm(posts~.,data=mydata,family=poisson)
summary(poisson.res)

y = mydata$posts
yhat = predict(poisson.res,type="response")

#dispersion statistic(n-k-1로 나눈것)
sum((y-yhat)^2/yhat)/(nrow(mydata)-length(coef(poisson.res)))
#결과가 5.169 (1과 차이大 → overdispersion 없는것 => nbm써야댐)
#(overdispersion있으면 포아송대신 negative binomial model 써야댐. 
# 실무적으로는 1.2~2 : 약한 과산포
# 2~4 : 중간과산포
# 4~ : 심한과산포라고 함

nb.res = glm.nb(posts~., data=mydata) #negative binomial model 적용
summary(nb.res)
#서머리 결과 쎄타가 0.11547으로, 1보다 심각히작으므로(variance는 1/쎄타) overdispersion 명백함

cbind(AIC(poisson.res),AIC(nb.res))
cbind(BIC(poisson.res),BIC(nb.res))
#포아송이랑 nb의 AIC/BIC값(25500 vs 16300 수준) 은 크게차이남(데이터셋이 overdispersion 문제임)

xbar = colMeans(mydata)[2:5] #calculate the mean values of all x variables (1은 y)

xb.pos = crossprod(coef(poisson.res),c(1,xbar)) #b0 + b1 * x1 + b2 * x2 + ... + bk * xk
xb.nb = crossprod(coef(nb.res),c(1,xbar)) #b0 + b1 * x1 + b2 * x2 + ... + bk * xk

k= 0:20
p1= dpois(k,exp(xb.pos)) #pdf of poisson distribution, mean parameter가 exp(xb.pos)
p2= dnbinom(k,size=nb.res$theta,mu=exp(xb.nb)) 
#pdf of negative binomial distribution, 얘는 mean parameter 뿐 아니라 dispersion parameter(size)도 존재

plot(k,p2,pch=16,xlab='visits',ylab='prob') #negative binomial model(pch는 point character)

points(k,p1) #poisson model거 추가비교
legend('topright',c('poisson','negative binomial'),pch=c(1,16))

#cf) 만약 데이터셋이 과산포가 있어도, 기대값(평균)만관심있다면 포아송모델 걍써도됨(nb model굳이안쓰고)
#=> 분산값이 평균보다커도(과산포있어도) 포아송모델의 평균추정치(MLE)는 일관성있음