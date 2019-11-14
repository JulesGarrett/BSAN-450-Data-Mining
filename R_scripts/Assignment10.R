library(TSA)
drunk = read.csv("Drunkintakes.csv")

#Plot times series for Intakes and difference in intakes
plot(drunk$Intakes, main='Drunkenness Intakes' ,ylab='Intakes ',xlab='Time',type='o')
plot(diff(drunk$Intakes), main='Difference in Drunkenness Intakes' ,ylab=' Difference in Intakes ',xlab='Time',type='o')


acf(drunk$Intakes,ci.type='ma')
pacf(drunk$Intakes,)

#Plot MA and AR for Difference intakes
acf(diff(drunk$Intakes),ci.type='ma')
pacf(diff(drunk$Intakes))

fit = arima(drunk$Intakes,order =c(1,1,1))
fit

#check diagnostics
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))
Box.test(residuals(fit),lag=10,type="Ljung",fitdf=2)

#P-value is too small test for tranformation and test again
fit1=BoxCox.ar(y=drunk$Intakes)

#create log var
drunk$log.int = log(drunk$Intakes)
plot(drunk$Intakes, main='Log Drunkenness Intakes' ,ylab='Log of Intakes ',xlab='Time',type='o')
plot(diff(drunk$Intakes), main='Log Difference in Drunkenness Intakes' ,ylab=' Log of Difference in Intakes ',xlab='Time',type='o')

#Plot MA and AR for Difference intakes
acf(diff(drunk$log.int),ci.type='ma')
pacf(diff(drunk$log.int))

l.fit = arima(drunk$log.int,order =c(0,1,1))
l.fit

#check diagnostics
plot(rstandard(l.fit),ylab='Standardized Residuals',type='o')
hist(rstandard(l.fit))
qqnorm(residuals(l.fit))
qqline(residuals(l.fit))
acf(rstandard(l.fit))
Box.test(residuals(l.fit),lag=10,type="Ljung",fitdf=1)

#create log var
drunk$sq.int = sqrt(drunk$Intakes)
plot(drunk$Intakes, main='Sqrt Drunkenness Intakes' ,ylab='Sqrt of Intakes ',xlab='Time',type='o')
plot(diff(drunk$Intakes), main='Sqrt Difference in Drunkenness Intakes' ,ylab=' Sqrt of Difference in Intakes ',xlab='Time',type='o')

#Plot MA and AR for Difference intakes
acf(diff(drunk$sq.int),ci.type='ma')
pacf(diff(drunk$sq.int))

sq.fit = arima(drunk$log.int,order =c(0,1,1))
sq.fit

#check diagnostics
plot(rstandard(l.fit),ylab='Standardized Residuals',type='o')
hist(rstandard(l.fit))
qqnorm(residuals(l.fit))
qqline(residuals(l.fit))
acf(rstandard(l.fit))
Box.test(residuals(l.fit),lag=10,type="Ljung",fitdf=1)

