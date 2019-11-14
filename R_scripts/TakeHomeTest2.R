library(TSA)
#read in the data
air = read.csv("Airfreight.csv")

#plot the data
plot(air$Ton.miles,type='o')
acf(air$Ton.miles,lag.max=36,ci.type='ma')
pacf(air$Ton.miles,lag.max=36,ci.type='ma')

#data not stationary: take first difference
plot(diff(air$Ton.miles),type='o')
acf(diff(air$Ton.miles),lag.max=36,ci.type='ma')
pacf(diff(air$Ton.miles),lag.max=36,ci.type='ma')

#ACF indicates a seasonal pattern: take 12th difference
plot(diff(air$Ton.miles, lag=12),type='o')
acf(diff(air$Ton.miles, lag=12),lag.max=36,ci.type='ma')
pacf(diff(air$Ton.miles, lag=12),lag.max=36,ci.type='ma')

#ACF indicates 1st differnce needed: take 1st and 12th difference
plot(diff(diff(air$Ton.miles), lag=12),type='o')
acf(diff(diff(air$Ton.miles), lag=12),lag.max=36,ci.type='ma')
pacf(diff(diff(air$Ton.miles), lag=12),lag.max=36,ci.type='ma')

#fit ARIMA model MA(2)*SMA(1)
fit=arima(air$Ton.miles,order=c(0 , 1 , 1),seasonal=list(order=c(0 , 1 , 1),period=12))
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

#removing outlier detected in AO
AO112=1*(seq(air$Ton.miles)==112)
xreg=data.frame(AO112)
fit1=arimax(air$Ton.miles,order=c(0 , 1 , 1), seasonal=list(order=c(0 , 1 , 1),xreg,period=12))
fit1

#removing outlier detected IO
fit2=arimax(air$Ton.miles,order=c(0 , 1 , 1),seasonal=list(order=c(0 , 1 , 1),period=12),io=c(112))
fit2

#IO outlier removed gave a better model
#test the fit again
plot(rstandard(fit2),ylab='Standardized Residuals',type='o')
hist(rstandard(fit2))
qqnorm(residuals(fit2))
qqline(residuals(fit2))
acf(rstandard(fit2),lag.max=36)
Box.test(residuals(fit2),lag=36,type="Ljung",fitdf=2)

#detect outliers
detectAO(fit2)
detectIO(fit2)









###############################################################
##################### QUESTION 2 ##############################
###############################################################
#read in the data
sio = read.csv("SIO.csv")

#plot the data
plot(sio$Ship,type='o')
acf(sio$Ship,lag.max=36,ci.type='ma')
pacf(sio$Ship,lag.max=36,ci.type='ma')

#data not stationary: take first difference
plot(diff(sio$Ship),type='o')
acf(diff(sio$Ship),lag.max=36,ci.type='ma')
pacf(diff(sio$Ship),lag.max=36,ci.type='ma')

#ACF indicates a seasonal pattern: take 12th difference
plot(diff(sio$Ship, lag=12),type='o')
acf(diff(sio$Ship, lag=12),lag.max=36,ci.type='ma')
pacf(diff(sio$Ship, lag=12),lag.max=36,ci.type='ma')

#ACF indicates 1st differnce needed: take 1st and 12th difference
plot(diff(diff(sio$Ship), lag=12),type='o')
acf(diff(diff(sio$Ship), lag=12),lag.max=36,ci.type='ma')
pacf(diff(diff(sio$Ship), lag=12),lag.max=36,ci.type='ma')

#fit a transformation
fit_trans=BoxCox.ar(y=sio$Ship,method="yule-walker")

#create inverse variable
sio$inv = 1/(sio$Ship)
sio$log = log(sio$Ship)

#startover with plotting data and looking at differences
#plot the data
plot(sio$inv,type='o')
acf(sio$inv,lag.max=36,ci.type='ma')
pacf(sio$inv,lag.max=36,ci.type='ma')

#data not stationary: take first difference
plot(diff(sio$inv),type='o')
acf(diff(sio$inv),lag.max=36,ci.type='ma')
pacf(diff(sio$inv),lag.max=36,ci.type='ma')

#ACF indicates a seasonal pattern: take 12th difference
plot(diff(sio$inv, lag=12),type='o')
acf(diff(sio$inv, lag=12),lag.max=36,ci.type='ma')
pacf(diff(sio$inv, lag=12),lag.max=36,ci.type='ma')

#ACF indicates 1st differnce needed: take 1st and 12th difference
plot(diff(diff(sio$inv), lag=12),type='o')
acf(diff(diff(sio$inv), lag=12),lag.max=36,ci.type='ma')
pacf(diff(diff(sio$inv), lag=12),lag.max=36,ci.type='ma')


#fit ARIMA model MA(3)*SAR(1)
fit=arima(sio$inv,order=c(0 , 1 , 3),seasonal=list(order=c(1 , 0 , 0),period=12))
fit

#check the fit of the model
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit),lag.max=36)
Box.test(residuals(fit),lag=36,type="Ljung",fitdf=4)

#detect outliers
detectAO(fit)
detectIO(fit)

#removing outlier detected in AO
AO=1*(seq(sio$inv) ==13 | seq(sio$inv) ==26)
AO
xreg=data.frame(AO)
fit1=arimax(sio$inv,order=c(0 , 1 , 3), seasonal=list(order=c(1 , 0 , 0),xreg,period=12))
fit1

#removing outlier detected IO
fit2=arimax(sio$inv,order=c(0 , 1 , 3),seasonal=list(order=c(1 , 0 , 0),period=12),io=c(12))
fit2

#IO outlier removed gave a better model
#test the fit again
plot(rstandard(fit1),ylab='Standardized Residuals',type='o')
hist(rstandard(fit1))
qqnorm(residuals(fit1))
qqline(residuals(fit1))
acf(rstandard(fit1),lag.max=36)
Box.test(residuals(fit1),lag=36,type="Ljung",fitdf=4)

#detect outliers
detectAO(fit1)
detectIO(fit1)

#removing outlier detected IO
#removing outlier detected in AO
fit3=arimax(sio$inv,order=c(3 , 1 , 0),seasonal=list(order=c(1 , 0 , 0),period=12),io=c(12))
fit3
plot(rstandard(fit3),ylab='Standardized Residuals',type='o')
hist(rstandard(fit3))
qqnorm(residuals(fit3))
qqline(residuals(fit3))
acf(rstandard(fit3),lag.max=36)
Box.test(residuals(fit3),lag=36,type="Ljung",fitdf=4)


#detect outliers
detectAO(fit3)
detectIO(fit3)

#remove outliers again
fit4=arimax(sio$inv,order=c(3 , 1 , 0),seasonal=list(order=c(1 , 0 , 0),period=12),io=c(12, 22))
fit4
plot(rstandard(fit4),ylab='Standardized Residuals',type='o')
hist(rstandard(fit4))
qqnorm(residuals(fit4))
qqline(residuals(fit4))
acf(rstandard(fit4),lag.max=36)
Box.test(residuals(fit4),lag=36,type="Ljung",fitdf=4)


#detect outliers
detectAO(fit4)
detectIO(fit4)






####
#QUESTION 3
####

sales = read.csv("DeptStoreSales.csv")
#plot the data
plot(sales$Sales,type='o')
acf(sales$Sales,lag.max=36,ci.type='ma')
pacf(sales$Sales,lag.max=36,ci.type='ma')

#1st difference
plot(diff(sales$Sales),type='o')
acf(diff(sales$Sales),lag.max=36,ci.type='ma')
pacf(diff(sales$Sales),lag.max=36,ci.type='ma')

#ACF indicates a seasonal pattern: take 12th difference
plot(diff(sales$Sales, lag=12),type='o')
acf(diff(sales$Sales, lag=12),lag.max=36,ci.type='ma')
pacf(diff(sales$Sales, lag=12),lag.max=36,ci.type='ma')

#ACF indicates 1st differnce needed: take 1st and 12th difference
plot(diff(diff(sales$Sales), lag=12),type='o')
acf(diff(diff(sales$Sales), lag=12),lag.max=36,ci.type='ma')
pacf(diff(diff(sales$Sales), lag=12),lag.max=36,ci.type='ma')

#fit a MA(1) model
fit=arimax(sales$Sales,order=c(0 , 1 , 1),seasonal=list(order=c(0 , 1 , 1),
                                                 period=12), method = "ML")
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

#create vars at key times

J91 = c(rep(0, 120), rep(1, 72))
Dec = c(rep(0,131), 1, rep(0,11), 1, rep(0,11), 1, rep(0,11), 1, rep(0,11), 1, rep(0,11), 1)
Nov = c(rep(0,130), 1, rep(0,11), 1, rep(0,11), 1, rep(0,11), 1, rep(0,11), 1, rep(0,11), 1, 0)
Oct = c(rep(0,141), 1, rep(0,11), 1, rep(0,11), 1, rep(0,11), 1, rep(0,11), 1, 0, 0)
Sept = c(rep(0,140), 1, rep(0,11), 1, rep(0,11), 1, rep(0,11), 1, rep(0,11), 1, 0, 0, 0)
Aug = c(rep(1,187), 1, 0,0,0,0)

fit=arimax(sales$Sales,order=c(0,1,1),seasonal=list(order=c(0,1,0),
          period=12),method='ML',xtransf=data.frame(J91, Oct, Aug),
           transfer=list(c(0,0),c(0,0),c(0,0) ))
fit
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit),lag.max=36)
Box.test(residuals(fit),lag=36,type="Ljung",fitdf=1)

detectAO(fit)
detectIO(fit)

