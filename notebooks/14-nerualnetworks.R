## 뉴럴네트워크를 R로 구현(hidden layer 1개버전+2개버전, 벤치마크로 로지스틱회귀와 예측정확성비교)
## nn은 각 variable과 상관성해석이어렵지만 예측정확도는 높음. 로지스틱회귀는 해석은가능하지만 예측정확도가 낮음 ("prediction accuracy" 와 "interpretability"간 tradeoff)

#install.packages("neuralnet") 
library(neuralnet) 

mydata <- read.csv("Smarket.csv") 
mydata <- mydata[,-c(1,8)] 
#Remove "Year", "Today") 
mydata

mydata[,-7] <- scale(mydata[,-7]) 
# only scale numerical variables(do not scale binary categorical variable 'Up') 

n.train <- floor(nrow(mydata)*0.8) # 80% training / 20% test 
set.seed(1000) 
ind.train <- sample(1:nrow(mydata),n.train) 
data.train <- mydata[ind.train,] 
data.test <- mydata[-ind.train,] 


## [1] variable 3개만 이용해서 심플하게 먼저수행
#hidden은 matrix아닌 정수꼴로 썼으므로 layer 1개/activation nodes개수 2개, linear을 FALSE로 해서 logistic/sigmoid function으로
#layer 여러개 쓰려면 C(x,y,z,...) 이런꼴로써야댐
nn <- neuralnet(Up ~ Lag1+Lag2, data = data.train, hidden = 2 , linear.output=FALSE) 

#시드설정해도 randomness때문에 result달라질수 있음(다운로드받은파일이용)
load("Smarket_nn1.Rda") 
plot(nn,rep='best') 

nn$weights 
# first level의 첫번째 weight는 intercept(bias), next two weights는 weights for the two x variables.
# second level은 output layer.(one output node만 있어서 여기결과는 하나의열)

data.test[1,] 
#   Lag1        Lag2       Lag3       Lag4        Lag5        Volume    Up
#‐0.5516457  0.9047775  0.8406793  0.3331338  ‐0.1722013  ‐0.5614015  TRUE

# calculate value of first activation node in the hidden layer
# linear combination of three input node values using the set of weights for the first activation node
s1 <- (-1.5526810)+(-0.4006151)*(-0.5516457)+(0.3979986)*(0.9047775)

# apply "non-linear activation function" or the G function (logistic or sigmoid function)
s1 <- exp(s1)/(1+exp(s1)) 
s1

# calculate value of second activation node in the hidden layer
s2 <- (-20.092955)+( 1.325688)*(-0.5516457)+(33.606746)*(0.9047775)
s2 <- exp(s2)/(1+exp(s2)) 
s2

# calculate value of the output node
p1 <- (-0.2670554)+(2.5133068)*s1+(-0.7918837)*s2
p1 <- exp(p1)/(1+exp(p1)) 
p1
#calculated value of the output node는 0.409로, 
#predicted probability of this particular player receiving high pay는 40.9% (cf. 그러나 실제로label은 TRUE였음)

#이노가다를 해주는함수 compute
compute(nn,data.test[1,])
# first level corresponds to the input layer
# second level corresponds to the hidden layer
# last level corresponds to the output layer


## [2] more complex neural network (variable 16개 모두포함, hidden layer 두개(각각 4,2개의 nodes))
nn <- neuralnet(Up ~ ., data = data.train, hidden = c(4,2) , linear.output=FALSE) 

load("Smarket_nn2.Rda")
plot(nn,rep='best') 

nn$weights 
# first level은 first hidden layer: 첫번째 weight는 intercept(bias), 다음 6개 weights는 Lag1~5과 Volume에 대한 weight
# second level은 second hidden layer: two columns corresponding to two activation nodes. 첫번째 weight는 intercept(bias), next 4개 weights는 weights for four nodes in that first hidden layer
# last level은 output layer: one column containing three values, corresponding to that single output node with intercept and two weights for the two nodes in the previous hidden layer, 

#모든 선수에 대해 Up예측
pred <- compute(nn, data.test)

# values represent the predicted probability of each player receiving high pay.(1에 가까울수록 높은연봉 확률높음)
pred$net.result

pred.class <- rep(FALSE, nrow(data.test))
pred.class[pred$net.result>0.5] <-TRUE

confusion <-table(pred.class, data.test$Up)
sum(diag(confusion)) / sum(confusion)
#nn은 54%정확성

#save(nn, file="Smarket_nn3.Rda") >> 결과 저장하고싶으면이용

## [3] logistic regression 결과와 비교(benchmark)
logit.res <- glm(Up~., data=data.train, family=binomial(link=logit))
# nn에는 clear interpretation이 없지만 회귀에서는 계수로 상관성 확인가능
summary(logit.res)


logit.pred.prob <- predict(logit.res, data.test,type="response")
logit.pred <- rep(FALSE, nrow(data.test))
logit.pred[logit.pred.prob>0.5] <- TRUE

confusion <-table(logit.pred, data.test$Up)
sum(diag(confusion)) / sum(confusion)
#logistic regression은 51.6%정확성
