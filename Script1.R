price = read.csv("HousePrices.csv")
plot(price$Price~price$SqFt)
plot(price$Price~price$Bed)
plot(price$Price~price$Bath)
plot(price$Price~price$Offers)
boxplot(price$Price~price$Brick,main="Brick (Yes/No)")
boxplot(price$Price~price$Nbrhood,main="Neighborhood")


boxplot(auto$mpg~auto$year)
plot(auto$mpg~auto$year)




fit=lm(Price~SqFt+Bed+Bath+Offers+Brick+Nbrhood,data=price)
summary(fit)
plot(fit$res~fit$fitted)
plot(fit$res~price$SqFt)
plot(fit$res~price$Bed)
plot(fit$res~price$Bath)
plot(fit$res~price$Offers)
boxplot(fit$res~price$Brick)
boxplot(fit$res~price$Nbrhood)
hist(fit$res)
qqnorm(fit$res)
shapiro.test(fit$res)

library(MASS)
textile = read.csv("textile.csv")
boxcox(Cycles~Length+Amplitude+Load,data=textile)


wind$sqrwind=sqrt(wind$Wind.Velocity)
textile$logtex = log(textile$Cycles)
attach(textile)
plot(logtex~Length,data=textile)
plot(logtex~Amplitude,data=textile)
plot(logtex~Load,data=textile)
fit_3 = lm(logtex~Length+Amplitude+Load,data=textile)
summary(fit_3)
plot(fit_3$residuals~fit_3$fitted.values)
hist(fit_3$residuals)
qqnorm(fit_3$residuals)
shapiro.test(fit_3$residuals)




order = read.csv("orderfulfillment.csv")
plot(order$Ave.Res.Time~order$Order.Volume)
order$time = c(1:50)
fit = lm(Ave.Res.Time~Order.Volume, data = order)
summary (fit)
fit1 = lm(Ave.Res.Time~Order.Volume+time, data = order)
summary(fit1)
plot(fit$res~order$time)
plot(fit$res~fit$fitted.values)
hist(fit$residuals)
qqnorm(fit$residuals)
shapiro.test(fit$residuals)
order$S31=1*(seq(order$Ave.Res.Time)>=31)
plot(order$Ave.Res.Time~order$Order.Volume+order$S31)
fit3 = lm(Ave.Res.Time~Order.Volume+S31, data = order)
summary (fit3)

plot(fit3$res~order$Order.Volume)
plot(fit3$res~order$time)
plot(fit3$res~fit$fitted.values)
hist(fit3$residuals)
qqnorm(fit3$residuals)
shapiro.test(fit3$residuals)

auto = read.csv("Auto.csv")
plot(auto$mpg~auto$weight)
library(boot)
set.seed(1)
glm.fit= glm(mpg~poly(weight, 1), data=auto)
cv.error = cv.glm(auto, glm.fit, K =10)$delta[1]
cv.error
#--------------------------------
glm.fit= glm(mpg~poly(weight, 2), data=auto)
cv.error = cv.glm(auto, glm.fit, K =10)$delta[1]
cv.error
#--------------------------------
glm.fit= glm(mpg~poly(weight, 3), data=auto)
cv.error = cv.glm(auto, glm.fit, K =10)$delta[1]
cv.error
#--------------------------------
glm.fit= glm(mpg~poly(weight, 4), data=auto)
cv.error = cv.glm(auto, glm.fit, K =10)$delta[1]
cv.error


fit4 = lm(mpg ~ poly(weight, 2), data = auto)
summary(fit4)
plot(fit4$res~auto$weight)
plot(fit4$res~fit4$fitted.values)
hist(fit4$residuals)
qqnorm(fit4$residuals)
shapiro.test(fit4$residuals)


library(boot)
set.seed(1)
plot(auto$mpg~auto$horsepower)
glm.fit= glm(mpg~poly(horsepower, 1), data=auto)
cv.error = cv.glm(auto, glm.fit, K =10)$delta[1]
cv.error
#--------------------------------
glm.fit= glm(mpg~poly(horsepower, 2), data=auto)
cv.error = cv.glm(auto, glm.fit, K =10)$delta[1]
cv.error
#--------------------------------
glm.fit= glm(mpg~poly(horsepower, 3), data=auto)
cv.error = cv.glm(auto, glm.fit, K =10)$delta[1]
cv.error
#--------------------------------
glm.fit= glm(mpg~poly(horsepower, 4), data=auto)
cv.error = cv.glm(auto, glm.fit, K =10)$delta[1]
cv.error

fit5 = lm(mpg~poly(horsepower, 2), data = auto)
summary (fit5)
plot(fit5$res~auto$horsepower)
plot(fit5$res~fit5$fitted.values)
hist(fit5$residuals)
qqnorm(fit5$residuals)
shapiro.test(fit5$residuals)
