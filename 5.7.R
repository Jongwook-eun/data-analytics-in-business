#install.packages("survival") 
library(survival) 
mydata =read.csv("Repurchase.csv") 

surv.res=survreg(Surv(Repurchase,1-Censored)~.-CustomerID, data=mydata, dist="weibull") 
#Surv function은 time duration("Repurchase"),indicator("1-Censored") 먼저 정의해줘야함 
#Surv function의 incicator은 0이면 right-censored, 1이면 not censored인데 
#데이터셋컬럼설정에서 값이 1이면 right-censored로 되어있어서, surv function의 indicator 은 "1-Censored"로 설정 
#distribution은 baseline hazard function 의미 => 우리는 hazard function으로 weibull function 이용 

summary(surv.res) #survreg결과로 나온 coefficients(delta)들은 의미있는값아니라서 transformation 해줘야함 

a = 1/surv.res$scale #take the inverse : shape paramater of the distribtuion 
a
beta = -coef(surv.res)*a 
beta 
# original beta coefficients 구하는 과정 
#proportional hazard models 에서 구한 beta계수들은 semi-elasticity interpretation을 가짐
#promotion variable의 계수가 0.17이라는 말은, customer가 promotional offer을 받았을때(0-1 사이의값)
#hazard rate(instantaeneous probability density of repurchase occuring)이 17% 상승한다는의미
#또 promotion의 delta계수(-0.202)와 beta계수(0.17)은 부호가 반대인데,
#이는 hazard rate이 increase할수록 "expected duration before repurchase" 가 decrease한다는 의미

#plot the hazard function
xbar = colMeans(mydata[,2:4]) #calculate the mean x values (2,3,4는 three x variables of the data)
xbeta = crossprod(c(1,xbar),beta) #calculate the usual xBeta combination (intercept에 대응하는게 1)

#hazard function은 function of time T
#curve function은 function of a certain independent variable as a smooth curve
curve(exp(c(xbeta))*a*x^(a-1),xlim=c(0,20),xlab="time",ylab="hazard")
#independent variable needs to be specified as "x", so the curve function knows which one is the independent variable
#Weibull model의 PH(Proportional Hazard)은 e^xbeta*a*t^(a-1)임. curve function의 independent variable "x"는 t(time) 의미

#a값은 0.84 >> 1보다 작으므로 negative duration dependence
#longer individual remains in the current state >> lower the probability it will switch into the other state
#∴hazard function실행시켜보면 time 커질수록 감소하는 형태

#plot the density function of duration distribution : dweibull function을 통해 Weibull distribution의 PDF도출가능
b=exp(-xbeta/a)
curve(dweibull(x, shape=a, scale=b),xlab="Repurchase Time",ylab="density",xlim=c(0,300),ylim=c(0,0.032))
#a값이 1보다 작아 negative duration dependence가 있으므로 t값작을때 hazard rate은 매우 높음
#반면 many observations이 short durations으로 끝나므로 probability of observing longer durations 은 작음
#따라서 duration이 길어질수록 density는 작아짐

#duration density function의 concept을 이용하기위해 히스토그램 이용
#right-censored data는 이용하면 안됨(right-censored의 duration은 실제duration값이 아니므로)
#frequency=FALSE 함으로써 y-axis가 frequency counts가 아니라 probability density 가 되도록
#add=TRUE 함으로써 current plot에 histogram 추가
hist(mydata$Repurchase[mydata$Censored==0],breaks=50,freq=FALSE, add=TRUE, col=NULL)

#histogram of actual durations observed in the data를 통해 (히스토그램)
#probability of observing durations in different lenses를 추정할수있음 (감소함수)


