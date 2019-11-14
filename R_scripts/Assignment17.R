loan=read.csv("LoanAccept.csv")
# change the type of the variables that are categorical.
loan$Response=factor(loan$Response)
loan$Educ=factor(loan$Educ)
loan$SecAcc=factor(loan$SecAcc)
loan$CD=factor(loan$CD)
loan$Online=factor(loan$Online)
set.seed(1)
train=sample(5000,4000)
test=(c(1:5000)[-train])
loantrain=loan[train,]
loantest=loan[test,]



library(rpart)
library(rpart.plot)
m1=rpart(Response~.,data=loantrain,method="class")


printcp(m1)
plotcp(m1)
#plot the full tree
rpart.plot(m1,type=3,extra=1)



##plot the pruned tree
m2=prune(m1,cp=.018088)
rpart.plot(m2,type = 3,extra=1)




library(ROCR)
prob=predict(m2,loantrain,type="prob")
pred=prediction(prob[,2],loantrain$Response)
perf = performance(pred ,"tpr","fpr")
plot(perf,main="ROC plot")
perf=performance(pred,"fpr","cutoff")
plot(perf)
perf=performance(pred,"fnr","cutoff")
plot(perf)
















icu=read.csv("icu.csv")
library(car)
icu$LOC1=icu$LOC
icu$LOC1=recode(icu$LOC1, "'stu'='com'")
set.seed(1)
train=sample(200,150)
test=(c(1:200)[-train])
icutrain=icu[train,]
icutest=icu[test,]

library(rpart)
library(rpart.plot)
m1=rpart(STA~.,data=icutrain,method="class")


printcp(m1)
plotcp(m1)
#plot the full tree
rpart.plot(m1,type=3,extra=1)



##plot the pruned tree
m2=prune(m1,cp=.03333333334)
rpart.plot(m2,type = 3,extra=1)




library(ROCR)
prob=predict(m2,icutrain,type="prob")
pred=prediction(prob[,2],icutrain$STA)
perf = performance(pred ,"tpr","fpr")
plot(perf,main="ROC plot")
perf=performance(pred,"fpr","cutoff")
plot(perf)
perf=performance(pred,"fnr","cutoff")
plot(perf)

