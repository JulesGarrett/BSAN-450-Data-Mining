library(TSA)
#Read in Data
spending = read.csv("ConstructionSpending.csv")

#plot Data and Data differences
plot(spending$Private,type='o')
acf(spending$Private,lag.max=36,ci.type='ma')
pacf(spending$Private,lag.max=36,ci.type='ma')

#plot the first difference
plot(diff(spending$Private),type='o')
acf(diff(spending$Private),lag.max=36,ci.type='ma')
pacf(diff(spending$Private),lag.max=36,ci.type='ma')

#plot the 12th difference
plot(diff(spending$Private,lag=12),type='o')
acf(diff(spending$Private,lag=12),lag.max=36,ci.type='ma')
pacf(diff(spending$Private,lag=12),lag.max=36,ci.type='ma')

#plot the 1st and 12th difference
plot(diff(diff(spending$Private),lag=12),type='o')
acf(diff(diff(spending$Private,lag=12)),lag.max=36,ci.type='ma')
pacf(diff(diff(spending$Private,lag=12)),lag.max=36,ci.type='ma')

#fit an ARIMA model
fit=arima(spending$Private,order=c(1 , 1 , 0),seasonal=list(order=c(0 , 1 , 1),period=12))
fit

#check the fit of the model
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit),lag.max=36)
Box.test(residuals(fit),lag=36,type="Ljung",fitdf=2)

#detect outliers
detectAO(fit)
detectIO(fit)

AO75=1*(seq(spending$Private)==75)
xreg=data.frame(AO75)
fit1=arimax(spending$Private,order=c(0,1,1),xreg,seasonal=list(order=c(0,1,1),period=12))
fit1

fit1=arimax(spending$Private,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),io=c(75))
fit1

##########################################################
#########          DATASET 2           ##############
##########################################################

#Read in Data
milk = read.csv("Milk.csv")

#plot Data and Data differences
plot(milk$Production,type='o')
acf(milk$Production,lag.max=36,ci.type='ma')
pacf(milk$Production,lag.max=36,ci.type='ma')

#plot the first difference
plot(diff(milk$Production),type='o')
acf(diff(milk$Production),lag.max=36,ci.type='ma')
pacf(diff(milk$Production),lag.max=36,ci.type='ma')

#plot the 12th difference
plot(diff(milk$Production,lag=12),type='o')
acf(diff(milk$Production,lag=12),lag.max=36,ci.type='ma')
pacf(diff(milk$Production,lag=12),lag.max=36,ci.type='ma')

#plot the 1st and 12th difference
plot(diff(diff(milk$Production),lag=12),type='o')
acf(diff(diff(milk$Production,lag=12)),lag.max=36,ci.type='ma')
pacf(diff(diff(milk$Production,lag=12)),lag.max=36,ci.type='ma')

#fit an ARIMA model
fit=arima(milk$Production,order=c(0 , 1 , 1),seasonal=list(order=c(0 , 1 , 1),period=12))
fit

#check the fit of the model
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit),lag.max=36)
Box.test(residuals(fit),lag=36,type="Ljung",fitdf=2)

#detect outliers
detectAO(fit)
detectIO(fit)


fit1=arimax(milk$Production,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),io=c(109))
fit1
