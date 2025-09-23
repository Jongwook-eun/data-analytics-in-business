data = read.csv("Loan.csv",header=T)
data$Education=as.factor(data$Education)
str(data)

lpm.res=lm(Loan~.,data)
summary(lpm.res)
