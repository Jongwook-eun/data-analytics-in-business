#endogeneity(내생성) 문제있을때 >> IV(Instrumental Variable, 도구변수) 이용

#install.packages("AER")
library(AER)
mydata = read.csv("Education_data.csv")
str(mydata)

hist(mydata$wage,breaks=50)
hist(log(mydata$wage),breaks=50) #data가 right-skewed여서 로그취해서 symmetric만들어줌
#dependent variable에 로그취하면 coefficient뜻은 independent variable변할때 종속변수의 퍼센트변화를 의미하게됨

ols.res<- lm(log(wage)~educ+exper+I(exper^2),data=mydata)
#add a experience squared as an illustration to show how to account for possible nonlinear relationship between y and some x variable
#I써줘야 제대로 들어감(identity function)

summary(ols.res)
#이게 potential endogeneity(내생성)고려안한 모델
#결과에 따르면 one additional year of education could increase a person's expected hourly wage by about 9%.
#However, due to the potential endogeneity issue, this coefficient estimate might be biased

#requirements for IV(Instrumental variable) are "exogeneity" and "relevance".
#exogeneity: uncorrelated with the error term or the unobserved variables that cause the endogeneity problem.
#relevance: needs to be highly correlated with the endogenous independent variable.

#직접 2SLS (Two-Stage Least Squares)수행해보기
#먼저 endogeneous variable을 IV와 exogenous variable에 회귀함으로써 error term과의 내생성을 제거한 효과만을 발라냄
#이후 그 fitted된(내생성이 제거된) 효과를 endogeneous variable(내생성 존재하는)인 educ 대신 투입



## Case1: nearc4을 IV로 놓았을때
# 1단계: 내생변수(educ)를 IV와 외생변수에 회귀
stage1 <- lm(educ~nearc4+exper+I(exper^2),data=mydata)
summary(stage1)
# IV의 설명력이 충분히 높아야 하므로, 보통 F-test로 약한 도구변수(weak instrument) 여부를 확인함
# (여기서는 러프하게 T-test로 확인 가능)

# 2단계: 1단계에서 예측된 educ(내생성 제거된 값)을 사용해 종속변수에 회귀
stage2 <- lm(log(wage)~fitted(stage1)+exper+I(exper^2),data=mydata)
summary(stage2)

# 동일한 절차는 사실 AER 패키지의 ivreg() 함수로 한 번에 수행 가능
TSLS.res<- ivreg(log(wage)~educ+exper+I(exper^2)|nearc4+exper+I(exper^2),data=mydata)
summary(TSLS.res)

# OLS vs 2SLS 비교
cbind(coef(ols.res),coef(TSLS.res))

#결과보면 내생성제거안됐을때는 교육이 소득에 미치는 효과 9% 인데
#내생성 효과 IV로 제거하니까 26%가됨
#=> OLS는 교육의 효과를 과소추정(underestimate) 하고 있었음.
#2SLS로 내생성(endogeneity)을 제거하니 교육의 임금 효과가 실제로 더 큼이 드러남.


## Case2: fatheduc&motheduc을 IV로 놓았을때
# 1단계: 내생변수(educ)를 IV와 외생변수에 회귀
stage1 <- lm(educ~fatheduc+motheduc+exper+I(exper^2),data=mydata)
summary(stage1)
# IV의 설명력이 충분히 높아야 하므로, 보통 F-test로 약한 도구변수(weak instrument) 여부를 확인함
# (여기서는 러프하게 T-test로 확인 가능)

# 2단계: 1단계에서 예측된 educ(내생성 제거된 값)을 사용해 종속변수에 회귀
stage2 <- lm(log(wage)~fitted(stage1)+exper+I(exper^2),data=mydata)
summary(stage2)

# 동일한 절차는 사실 AER 패키지의 ivreg() 함수로 한 번에 수행 가능
TSLS.res<- ivreg(log(wage)~educ+exper+I(exper^2)|fatheduc+motheduc+exper+I(exper^2),data=mydata)
summary(TSLS.res)

# OLS vs 2SLS 비교
cbind(coef(ols.res),coef(TSLS.res))

#결과보면 내생성제거안됐을때는 교육이 소득에 미치는 효과 9% 인데
#내생성 효과 IV로 제거하니까 15%가됨
#=> OLS는 교육의 효과를 과소추정(underestimate) 하고 있었음.
#2SLS로 내생성(endogeneity)을 제거하니 교육의 임금 효과가 실제로 더 큼이 드러남.


## 결론
#underestimate하고있었다는 의미(downward bias)는 unobserved variable이 earning(response)과 education(endogeneous variable)에 미치는효과가 반대라는거
#Some unobserved variable in the error term positively contributes to earnings and is negatively correlated with the years of education.이거나
#Some unobserved variable in the error term negatively contributes to earnings and is positively correlated with the years of education.

#downward bias를 추측해보자면 “능력이 낮은 사람들이 그 약점을 메우기 위해 더 오래 공부한다"거나, 

# “부모의 사회적 자본이 낮은 사람이 교육을 더 받아야만 성공할 수 있다.” 등의 경우때문에 교육과 오차항이 음(-)의 상관이었던듯
