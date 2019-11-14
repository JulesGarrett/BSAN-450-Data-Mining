crash = read.csv("crash.csv")
#Graph all continuous Independent Variables vs Head Injury
plot(crash$HEAD_INJ~crash$CHEST_IN)
plot(crash$HEAD_INJ~crash$LLEG_INJ)
plot(crash$HEAD_INJ~crash$RLEG_INJ)
plot(crash$HEAD_INJ~crash$YEAR)
plot(crash$HEAD_INJ~crash$WEIGHT)


#Graph all catgorical Independent Variables vs Head Injury
#MAKE, DRIV_PAS, PROTECT, DOORS, and SIZE
boxplot(crash$HEAD_INJ~crash$MAKE)
boxplot(crash$HEAD_INJ~crash$DRIV_PAS)
boxplot(crash$HEAD_INJ~crash$PROTECT)
boxplot(crash$HEAD_INJ~crash$DOORS)
boxplot(crash$HEAD_INJ~crash$SIZE)

#part C
library(leaps)
crash1=na.omit(crash)
# use 10 fold cross validation
set.seed(11)
folds=sample(rep(1:10,length=nrow(crash1)))
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}
cv.errors=matrix(NA,10,5)
for(k in 1:10){
  best.fit=regsubsets(HEAD_INJ~CHEST_IN+LLEG_INJ+RLEG_INJ+WEIGHT+YEAR,data=crash1[folds!=k,],nvmax=5)
  for(i in 1:5){
    pred=predict(best.fit,crash1[folds==k,],id=i)
    cv.errors[k,i]=mean( (crash1$HEAD_INJ[folds==k]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch=19,type="b")
rmse.cv

#part d
fit1.full=regsubsets(HEAD_INJ~CHEST_IN+LLEG_INJ+RLEG_INJ+WEIGHT+YEAR,data=crash, nvmax = 5)
coef(fit1.full,2)
redmod = lm(HEAD_INJ~CHEST_IN+YEAR, data=crash)
fullmod = lm(HEAD_INJ~CHEST_IN+YEAR+SIZE,data=crash)
anova(fullmod,redmod)

#part f
#MAKE, DRIV_PAS, PROTECT, DOORS, and SIZE
fit = lm(HEAD_INJ~CHEST_IN+YEAR+DOORS, data = crash)
summary(fit)
plot(fit$residuals~fit$fitted.values)
