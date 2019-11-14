loan=read.csv("LoanAccept.csv")
loan$Response=factor(loan$Response)
loan$Educ=factor(loan$Educ)
loan$SecAcc=factor(loan$SecAcc)
loan$CD=factor(loan$CD)
loan$Online=factor(loan$Online)
loan$Fam=factor(loan$Fam)
set.seed(1)
train=sample(5000,4000)
test=(c(1:5000)[-train])
loantrain=loan[train,]
loantest=loan[test,]

#create histograms
library(lattice)
histogram(~Age|Response,data=loantrain,layout=c(1,2))

histogram(~Inc|Response,data=loantrain,layout=c(1,2))

#chi squared test 
chisq.test(loantrain$Response,loantrain$Fam)

#fit a Naive Bayes
x.vars= c(3,4,5,6,7,10)
x=loantrain[,x.vars]
y=loantrain$Response
library(naivebayes)
model=naive_bayes(x,y,usekernel=TRUE)
plot(model)


#plot ROC plot, false positive errror v cuffoff
# and flase negative v. cuttoff
library(ROCR)
prob=predict(model,x, type = "prob")
pred=prediction(prob[,2],y)
perf = performance(pred ,"tpr","fpr")
plot(perf,main="ROC plot")
perf=performance(pred,"fpr","cutoff")
plot(perf)
perf=performance(pred,"fnr","cutoff")
plot(perf)


#choose cutoff of 0.2 
prob=predict(model,x, type = "prob")
pred=ifelse(prob[,2]>.2,1,0)
table( pred,y)

xtest=loantest[,x.vars]
prob=predict(model,xtest, type = "prob")
pred=ifelse(prob[,2]>.2,1,0)
table( pred,loantest$Response)












##Question 2

icu=read.csv("icu.csv")
library(car)
icu$LOC1=icu$LOC
icu$LOC1=recode(icu$LOC1, "'stu'='com'")
set.seed(1)
train=sample(200,150)
test=(c(1:200)[-train])
icutrain=icu[train,]
icutest=icu[test,]
icu$STA = factor(icu$STA)

#SYS, HRA, AGE
histogram(~SYS|STA,data=icutrain,layout=c(1,2))
histogram(~HRA|STA,data=icutrain,layout=c(1,2))
histogram(~AGE|STA,data=icutrain,layout=c(1,2))

#Chi Squared
chisq.test(icutrain$STA,icutrain$GENDER)
chisq.test(icutrain$STA,icutrain$RACE)
chisq.test(icutrain$STA,icutrain$SER)
chisq.test(icutrain$STA,icutrain$CAN)
chisq.test(icutrain$STA,icutrain$CRN)
chisq.test(icutrain$STA,icutrain$INF)
chisq.test(icutrain$STA,icutrain$CPR)
chisq.test(icutrain$STA,icutrain$PRE)
chisq.test(icutrain$STA,icutrain$TYP)
chisq.test(icutrain$STA,icutrain$FRA)
chisq.test(icutrain$STA,icutrain$PO2)
chisq.test(icutrain$STA,icutrain$PH)
chisq.test(icutrain$STA,icutrain$PCO)
chisq.test(icutrain$STA,icutrain$BIC)
chisq.test(icutrain$STA,icutrain$CRE)
chisq.test(icutrain$STA,icutrain$LOC)
chisq.test(icutrain$STA,icutrain$LOC1)


#make vars factors 
icutest$CPR = factor(icutest$CPR)
icutrain$CPR = factor(icutrain$CPR)
icutest$TYP = factor(icutest$TYP)
icutrain$TYP = factor(icutrain$TYP)
icutest$LOC = factor(icutest$LOC)
icutrain$LOC = factor(icutrain$LOC)
icutest$LOC1 = factor(icutest$LOC1)
icutrain$LOC1 = factor(icutrain$LOC1)


#fit a Naive Bayes
x.vars= c(9,10,13,20,21)
x=icutrain[,x.vars]
y=icutrain$STA
library(naivebayes)
model=naive_bayes(x,y,usekernel=TRUE)
plot(model)


library(ROCR)
prob=predict(model,x, type = "prob")
pred=prediction(prob[,2],y)
perf = performance(pred ,"tpr","fpr")
plot(perf,main="ROC plot")
perf=performance(pred,"fpr","cutoff")
plot(perf)
perf=performance(pred,"fnr","cutoff")
plot(perf)


#choose cutoff of 0.5
prob=predict(model,x, type = "prob")
pred=ifelse(prob[,2]>.5,1,0)
table( pred,y)

xtest=icutest[,x.vars]
prob=predict(model,xtest, type = "prob")
pred=ifelse(prob[,2]>.5,1,0)
table( pred,icutest$STA)







