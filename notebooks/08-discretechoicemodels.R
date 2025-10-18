#Response가 세개이상일때(multimonimal) >> Multinomial logistic regression(다중로지스틱 회귀) 이용

#install.packages("mlogit") #multinomial logit모듈
library(mlogit)
mydata = read.csv("Commute_Mode.csv") #mode열이 4개의 response임 (multinomial)
str(mydata)

commute <-mlogit.data(mydata,shape='long',choice='choice',alt.var='mode',chid.var='id')
#data에는 long format /wide format이 존재함.wide format은 한행에 한 데이터의 모든 정보가 들어간 형태(그만큼 열이 넓어짐) 
#choice는 choice decision(response)을 담고있는 열
#alt.var은 names of the alternatives 담고있는 열
#chid.var은 id담고있는 열

head(commute)

ml.res<-mlogit(choice~cost+time,commute) #linear regression과 유사하게 dependent~independent
#simple cases, where there only involve individual and alternative specific variables > only consider common/generic coefficients
#cf)complex full model specification에서는 separate the independent variables into different parts using the pipe sine 해야함

summary(ml.res)
#첫 세개는 alternative specific intercepts (four commute system alternatives에서 (4-1)개-> bus는 baseline이여서 intercept 0로 normalized)
#positive coefficient의 의미 > baseline alternative(bus)에 비해 양수는 generate higher utilities on average
#negative coefficient의 의미 > baseline alternative(bus)에 비해 음수는 generate lower utilities on average
# cost와 time(둘다 individual and alternative specific) >> only assign a common generic coefficient in front of each of them.
# U_ij = α_j + β_ic X ic_ij + β_oc X oc_ij (i는 개인, j는 대안 >> 모든 대안에 대해 같은 계수 β_ic, β_oc (generic coefficient)를 사용하겠다는 뜻)
# coefficients are negative >> as the cost/time increase, the utility from any of the commute options will decrease

#U_EC = 0 + β_ic * ic_EC + β_oc * oc_EC  
#U_ER = 0.195 + β_ic * ic_ER + β_oc * oc_ER  
#U_GC = 0.052 + β_ic * ic_GC + β_oc * oc_GC  
#U_GR = -1.351 + β_ic * ic_GR + β_oc * oc_GR  
#U_HP = -1.659 + β_ic * ic_HP + β_oc * oc_HP
#β_ic = -0.00153  
#β_oc = -0.00699


head(fitted(ml.res, outcome=FALSE))
# predicated choice probability of each commute option

cost.avg<- tapply(commute$cost, commute$mode, mean)
# apply the mean function to the cost variable of the commute data by each different value of the alternative variable(mode)
# 대안(mode)에 따라 cost/time의 평균을 각각구해야함
time.avg<- tapply(commute$time, commute$mode, mean)

xval<- data.frame(cost=cost.avg, time=time.avg)
xval

effects(ml.res, covariate='time',data=xval)
#marginal effects
#diagonal cells measure the self marginal effects(ex: bus의 time이 one unit 증가했을때 bus 대안 probability증감 negative)
#off diagonal cells represent the cross marginal effects(ex: bus의 time이 one unit 증가했을때 car/carpool/rail의 probability증감 positive)

# 이케이스에서는  simple model specification (only consider a common Beta coefficient for all alternatives) 라서 symmetric matrix지만 
# 보통 model에서는 Beta coefficients are allowed to be alternative specific,

# >> not necessary for the marginal effect of j on j prime to be the same as that of j prime on j
