library(TSA)
#read in data
Temperature=read.csv("TemperatureChange.csv")

#plot the time series for temp change and difference in temp change
plot(Temperature$Change,ylab='Temperature Change',xlab='Time',type='o')
plot(diff(Temperature$Change),ylab='Differences in Temperature Change',xlab='Time',type='o')

#plot the sample ACF for temp change
acf(Temperature$Change,ci.type='ma')

#plot ACF and PACF for diff in temp change
acf(diff(Temperature$Change),ci.type='ma')
pacf(diff(Temperature$Change))

#fit a MA(1) model to the time series
fit=arima(diff(Temperature$Change),order =c(0,0,1))
fit

#check diagnostics
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=1)

#predict 
plot(fit,n.ahead=10,type='b')
predict(fit,n.ahead=10)

#######################DATASET #2##########################################

#read in data
Tool=read.csv("CuttingTool.csv")

#plot the time series for temp change and difference in temp change
plot(Tool$Sales,ylab='Sales',xlab='Time',type='o')
plot(diff(Tool$Sales),ylab='Differences in Sales',xlab='Time',type='o')

#plot the sample ACF for temp change
acf(Tool$Sales,ci.type='ma')

#plot ACF and PACF for diff in temp change
acf(diff(Tool$Sales),ci.type='ma')
pacf(diff(Tool$Sales))

#fit a MA(1) model to the time series
fit=arima(diff(Tool$Sales),order =c(0,1,1))
fit

#fit a AR(1) model to the time series to compare
fit1=arima(diff(Tool$Sales),order =c(1,1,0))
fit1

#use MA(1) bc better s.e.

#check diagnostics
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=1)



