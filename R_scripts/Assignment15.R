#read in the data
Donner = read.csv("Donner.csv")
fit=glm(Surv~ Age+MultFam,family=binomial, data=Donner)
prob=predict(fit,newdata=Donner,type="response")
pred=ifelse(prob>.5,1,0)
table( pred,Donner$Surv)


library(ROCR)
pre=prediction(prob,Donner$Surv)
perf = performance(pre,"tpr","fpr")
plot(perf,main="ROC plot")
perf=performance(pre,"fpr","cutoff")
plot(perf)
perf=performance(pre,"fnr","cutoff")
plot(perf)




###########
#QUESTION 2
###########


loan=read.csv("LoanAccept.csv")
set.seed(1)
train=sample(5000,4000)
test=(c(1:5000)[-train])
loan.train=loan[train,]
loan.test=loan[test,]

fit=glm(formula = Response ~ Inc + factor(Educ) + CD + Fam + Online + 
          CreditCard + CCAve + SecAcc, family = binomial, data = loan.train)
prob=predict(fit,loan.train, type ="response")
library(ROCR)
pred = prediction( prob,loan.train$Response)
perf = performance(pred,"tpr","fpr")
plot(perf,main="ROC plot")
perf=performance(pred,"fpr","cutoff")
plot(perf)
perf=performance(pred,"fnr","cutoff")
plot(perf)


fit=glm(formula = Response ~ Inc + factor(Educ) + CD + Fam + Online + 
          CreditCard + CCAve + SecAcc, family = binomial, data = loan.train)
prob=predict(fit,newdata=loan.train, type ="response")
pred=ifelse(prob>.2,1,0)
table( pred,loan.train$Response)

#fit with the test data
prob=predict(fit,newdata=loan.test, type ="response")
pred=ifelse(prob>.2,1,0)
table( pred,loan.test$Response)
