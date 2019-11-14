hw = read.csv('highway.csv')
plot(hw$Rate~hw$Len+hw$Adt+hw$Trks+hw$Slim+hw$Lwid+hw$Shld+hw$Itg+hw$Sigs+hw$Acpt+hw$Lane)
plot(hw$Rate~as.factor(hw$Fai)+as.factor(hw$Pa)+as.factor(hw$Ma))

fit = lm(hw$Rate~hw$Len+hw$Adt+hw$Trks+hw$Slim+hw$Lwid+hw$Shld+hw$Itg+
           hw$Sigs+hw$Acpt+as.factor(hw$Fai)+
           as.factor(hw$Pa)+as.factor(hw$Ma))
summary(fit)     

fit = lm(Rate~Len+Slim+Acpt, data = hw)
summary(fit)
plot(fit$res~fit$fitted.values)
hist(stdres(fit))
qqnorm(fit$residuals)
shapiro.test(fit_test$residuals)
#####################################################
#Question 2
#####################################################
power=read.csv("powerplant.csv")
install.packages("leaps")
library(leaps)
fit1.full=regsubsets(C~.,power, nvmax = 10)
summary(fit1.full)
reg.summary=summary(fit1.full)
reg.summary$adjr2
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq", type = "l")
reg.summary$cp
plot(reg.summary$cp,xlab="Number of Variables", ylab=" Cp", type = "l")
reg.summary$bic
plot(reg.summary$bic,xlab="Number of Variables", ylab=" BIC", type = "l")


fit1.fow=regsubsets(C~.,power, nvmax = 10, method ="forward") 
summary(fit1.fow)
reg.summary=summary(fit1.fow)
reg.summary$adjr2
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq", type = "l")
reg.summary$cp
plot(reg.summary$cp,xlab="Number of Variables", ylab=" Cp", type = "l")
reg.summary$bic
plot(reg.summary$bic,xlab="Number of Variables", ylab=" BIC", type = "l")
coef(fit1.fow,7)


fit1.bac=regsubsets(C~.,power, nvmax = 10, method ="backward") 
summary(fit1.bac)
reg.summary=summary(fit1.bac)
reg.summary$adjr2
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq", type = "l")
reg.summary$cp
plot(reg.summary$cp,xlab="Number of Variables", ylab=" Cp", type = "l")
reg.summary$bic
plot(reg.summary$bic,xlab="Number of Variables", ylab=" BIC", type = "l")
coef(fit1.fow,7)


fit2 = lm(C~D+T2+S+PR+NE+N+PT, data = power)
summary(fit2)
plot(fit2$residuals~fit2$fitted.values)
hist(stdres(fit2))
qqnorm(fit2$residuals)
shapiro.test(fit2$residuals)
