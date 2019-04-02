#Question 1
library(TSA)
#Read in the data
Portland=read.csv("PortlandBusRidership.csv")
dev.new(width=6.875, height = 3.5,pointsize=8)
#plot the data
plot(Portland$Riders,type='o')
acf(Portland$Riders,lag.max=36,ci.type='ma')
#plot the first difference
plot(diff(Portland$Riders),type='o')
acf(diff(Portland$Riders),lag.max=36,ci.type='ma')
#plot the 12th difference
plot(diff(Portland$Riders,lag=12),type='o')
acf(diff(Portland$Riders,lag=12),lag.max=36,ci.type='ma')
#plot the 1st and 12th difference
plot(diff(diff(Portland$Riders),lag=12),type='o')
acf(diff(diff(Portland$Riders,lag=12)),lag.max=36,ci.type='ma')

#fit an ARIMA model
fit=arima(Portland$Riders,order=c(0 , 1 , 0),seasonal=list(order=c(0 , 1 , 2),period=12))
fit
#check the fit of the model
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit),lag.max=36)
Box.test(residuals(fit),lag=36,type="Ljung",fitdf=2)

#predict the next 24 values
predict(fit,n.ahead=24)
plot(fit,n.ahead=24,type='b')


#Question 2
library(TSA)
#read in data
hs=read.csv("Starts.csv")

#plot the data
plot(hs$Starts,type='o')
acf(hs$Starts,lag.max=36,ci.type='ma')
#plot the first difference
plot(diff(hs$Starts),type='o')
acf(diff(hs$Starts),lag.max=36,ci.type='ma')
#plot the 12th difference
plot(diff(hs$Starts,lag=12),type='o')
acf(diff(hs$Starts,lag=12),lag.max=36,ci.type='ma')
#plot the 1st and 12th difference
plot(diff(diff(hs$Starts),lag=12),type='o')
acf(diff(diff(hs$Starts,lag=12)),lag.max=36,ci.type='ma')

#find the transformation of the data
fit1=BoxCox.ar(y=hs$Starts,method="yule-walker")

#take a log transformation of the data
hs$logStarts= log(hs$Starts)
#plot the data
plot(hs$logStarts,type='o')
acf(hs$logStarts,lag.max=36,ci.type='ma')
#plot the first difference
plot(diff(hs$logStarts),type='o')
acf(diff(hs$logStarts),lag.max=36,ci.type='ma')
#plot the 12th difference
plot(diff(hs$logStarts,lag=12),type='o')
acf(diff(hs$logStarts,lag=12),lag.max=36,ci.type='ma')
#plot the 1st and 12th difference
plot(diff(diff(hs$logStarts),lag=12),type='o')
acf(diff(diff(hs$logStarts,lag=12)),lag.max=36,ci.type='ma')

#fit MA(1)*SMA(1) model to the time series
fit=arima(hs$logStarts,order=c(0 , 1 , 1),seasonal=list(order=c(0 , 1 , 1),period=12))
fit
#MA(1) coeff not significant 
#fit SMA(1) model to the time series 
fit=arima(hs$logStarts,order=c(0 , 1 , 1),seasonal=list(order=c(0 , 1 , 1),period=12))
fit
#dianogstic checks
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit),lag.max=36)
Box.test(residuals(fit),lag=36,type="Ljung",fitdf=2)

#predict the next 24 values
predict(fit,n.ahead=24)
plot(fit,n.ahead=24,type='b')
