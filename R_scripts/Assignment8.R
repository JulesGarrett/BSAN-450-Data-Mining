#Load time series library
library(TSA)

#####################################################
################Question Set 1#######################
#####################################################

#Read in the data
concentration=read.csv("ChemicalConcentration.csv")

#plot the data, sample autocorrelations, and sameple partial sutocorrelations
plot(concentration$Con,ylab='Chemical Concentration Numbers',xlab='Time',type='o')
acf(concentration$Con,ci.type='ma')
pacf(concentration$Con)


#Estimate the parameters for the AR(2) model. 
fit=arima(concentration$Con,order =c(2,0,0))
fit

#check diagnostics for errors
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=2)


#Estimate the parameters for the ARMA(2,1) model. 
fit=arima(concentration$Con,order =c(2,0,1))
fit
#Estimate the parameters for the ARMA(3,0) model. 
fit=arima(concentration$Con,order =c(3,0,0))
fit

#computing/plot forecasts for AR(2) model
plot(fit,n.ahead=10,type='b')
predict(fit,n.ahead=10)

#####################################################
################Question Set 2#######################
#####################################################

#Read in the data
t=read.csv("time.csv")

#plot the data, sample autocorrelations, and sameple partial sutocorrelations
plot(t$Time,ylab='Percent of Time',xlab='Time',type='o')
acf(t$Time,ci.type='ma')
pacf(t$Time)


#Estimate the parameters for the ARMA(1,1) model. 
fit=arima(t$Time,order =c(1,0,1))
fit

#check diagnostics for errors
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=2)


#computing/plot forecasts for ARMA(1,1) model
predict(fit,n.ahead=10)
