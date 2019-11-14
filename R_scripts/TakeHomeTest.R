library(leaps)
library(MASS)
#SETTING UP THE DATA AND CLEANING
#Read in data as iowa
iowa_full = read.csv('AmesHousing.csv')

#omit missing values
iowa = na.omit(iowa_full)
                       
#create a training set and a testing set
set.seed(1)
train=sample(2258,1800)
test=(c(1:2258)[-train])

#FIND OUT HOW MANY VARIABLE
#split the data to only have numeric vars
num.iowa=data.frame(iowa[,c(2,3,14:17,23,31,33:35,40:49,51,53,56,58,59,63:68,70,71,74)])
fit_num = lm(SalePrice~., data = num.iowa, subset=train)
summary(fit_num)

#test how many variables should be included
fit_num.full = regsubsets(SalePrice~., data=num.iowa, nvmax=35)
summary(fit_num.full)
reg.summary = summary(fit_num.full)
reg.summary$adjr2
plot(reg.summary$adjr2, xlab="Number of Variables", ylab = "Adjusted RSq", type = "l")         
reg.summary$cp
plot(reg.summary$cp, xlab = "Number of Variables", ylab = " Cp", type ="l")
reg.summary$bic
plot(reg.summary$bic, xlab = "Number of Variables", ylab=" BIC", type = "l")
#find the variables that should be included for a X variable model
coef(fit_num.full, 20)

fit_num.part = lm(SalePrice~Lot.Frontage+Lot.Area + Overall.Qual + Overall.Cond + Year.Built + Year.Remod.Add+        
                  Mas.Vnr.Area + BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Bsmt.Full.Bath + 
                  Bsmt.Half.Bath  + Half.Bath + TotRms.AbvGrd +  Fireplaces + Garage.Yr.Blt + Wood.Deck.SF+
                  Open.Porch.SF + Enclosed.Porch + Misc.Val, data = num.iowa, subset= train)
summary(fit_num.part)
#adjust the model based off the pvalues from prev output
fit_num.part1 = lm(SalePrice~Lot.Area + Overall.Qual + Overall.Cond + Year.Remod.Add+        
                     Mas.Vnr.Area + BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath + TotRms.AbvGrd +  
                     Fireplaces + Garage.Yr.Blt + Open.Porch.SF + Enclosed.Porch,
                   data = num.iowa, subset= train)                            
summary(fit_num.part1)

#Add categorical variables to the model one at a time run anova command to decide if to add
#if close mentality is to add and can remove later if needed
model1 = lm(SalePrice~Lot.Area + Overall.Qual + Overall.Cond + Year.Remod.Add+        
              Mas.Vnr.Area + BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath + TotRms.AbvGrd +  
              Fireplaces + Garage.Yr.Blt + Open.Porch.SF + Enclosed.Porch +Lot.Shape + Land.Contour + Lot.Config +
              Land.Slope+ Condition.1 + Condition.2+ Bldg.Type+ House.Style+ Roof.Style+ Exterior.1st+Exterior.2nd+
              Mas.Vnr.Type+ Exter.Qual+Foundation+Bsmt.Qual+Bsmt.Exposure+ BsmtFin.Type.2+ Heating.QC+Kitchen.Qual+
              Functional+ Fireplace.Qu+Garage.Type+Sale.Type,
            data = iowa, subset= train)
model2 = lm(SalePrice~Lot.Area + Overall.Qual + Overall.Cond + Year.Remod.Add+        
              Mas.Vnr.Area + BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath + TotRms.AbvGrd +  
              Fireplaces + Garage.Yr.Blt + Open.Porch.SF + Enclosed.Porch + Lot.Shape + Land.Contour + Lot.Config +
              Land.Slope+ Condition.1 + Condition.2 + Bldg.Type + House.Style + Roof.Style + Exterior.1st + Exterior.2nd+
              Mas.Vnr.Type+ Exter.Qual +Foundation +Bsmt.Qual + Bsmt.Exposure + BsmtFin.Type.2 + Heating.QC+ Kitchen.Qual+
              Functional+ Fireplace.Qu+ Garage.Type+Sale.Type,
            data = iowa, subset= train)
anova(model1, model2)

#model with only statistically significant variables
fit_vipvars = lm(SalePrice~Lot.Area + Overall.Qual + Overall.Cond + Year.Remod.Add+        
                   Mas.Vnr.Area + BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath + TotRms.AbvGrd +  
                   Fireplaces + Garage.Yr.Blt + Open.Porch.SF + Enclosed.Porch + Lot.Shape + Land.Contour + Lot.Config +
                   Land.Slope+ Condition.1 + Condition.2 + Bldg.Type + House.Style + Roof.Style + Exterior.1st + Exterior.2nd+
                   Mas.Vnr.Type+ Exter.Qual +Foundation +Bsmt.Qual + Bsmt.Exposure + BsmtFin.Type.2 + Heating.QC+ Kitchen.Qual+
                   Functional+ Fireplace.Qu+ Garage.Type+Sale.Type,
                 data = iowa, subset= train)
summary(fit_vipvars)
#after looking at p-values remove some vars from the model
fit_vipvars = lm(SalePrice~Lot.Area + Overall.Qual + Overall.Cond + Year.Remod.Add+        
                   Mas.Vnr.Area + BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath + TotRms.AbvGrd +  
                   Fireplaces + Garage.Yr.Blt + Open.Porch.SF + Enclosed.Porch  + Land.Contour + Lot.Config +
                   Land.Slope+ Condition.1  + Bldg.Type + House.Style+
                   Mas.Vnr.Type+ Exter.Qual +Bsmt.Qual + Bsmt.Exposure + BsmtFin.Type.2 + Heating.QC+ Kitchen.Qual+
                   Functional+ Fireplace.Qu+ Garage.Type+Sale.Type,
                 data = iowa_noOut3)
summary(fit_vipvars)
#Look for issues in res v fitted and stdres
plot(fit_vipvars$residuals~fit_vipvars$fitted.values)
hist(stdres(fit_vipvars))
qqnorm(fit_vipvars$residuals)
shapiro.test(fit_vipvars$residuals)
#test to see how tranformation would look since residual looks cornicopia shaped
boxcox(fit_vipvars)
iowa$logPrice = log(iowa$SalePrice)
fit_vipvars2 = lm(logPrice~Lot.Area + Overall.Qual + Overall.Cond + Year.Remod.Add+        
                   Mas.Vnr.Area + BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath + TotRms.AbvGrd +  
                   Fireplaces + Garage.Yr.Blt + Open.Porch.SF + Enclosed.Porch  + Land.Contour + Lot.Config +
                   Land.Slope+ Condition.1  + Bldg.Type + House.Style+
                   Mas.Vnr.Type+ Exter.Qual +Bsmt.Qual + Bsmt.Exposure + BsmtFin.Type.2 + Heating.QC+ Kitchen.Qual+
                   Functional+ Fireplace.Qu+ Garage.Type+Sale.Type,
                 data = iowa, subset= train)
summary(fit_vipvars2)
plot(fit_vipvars2$residuals~fit_vipvars2$fitted.values)
hist(stdres(fit_vipvars2))
qqnorm(fit_vipvars2$residuals)
shapiro.test(fit_vipvars2$residuals)
#removed some variable that are not significant
fit_vipvars3 = lm(logPrice~Lot.Area + Overall.Qual + Overall.Cond + Year.Remod.Add+        
                      BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath + TotRms.AbvGrd +  
                    Fireplaces + Garage.Yr.Blt + Open.Porch.SF + Enclosed.Porch  + Land.Contour +
                    Land.Slope+ Condition.1  + Bldg.Type + House.Style+
                    Mas.Vnr.Type+ Exter.Qual +Bsmt.Qual + BsmtFin.Type.2 + Heating.QC+ Kitchen.Qual+
                    Functional + Garage.Type+Sale.Type,
                  data = iowa, subset= train)
summary(fit_vipvars3)
#run diagnostics on the model
plot(fit_vipvars3$residuals~fit_vipvars3$fitted.values)
hist(stdres(fit_vipvars3))
qqnorm(fit_vipvars3$residuals)
shapiro.test(fit_vipvars3$residuals)

#use testing data to see if model is a good fit
#also removed variables one at a time that were not statistically significant
test.fit_vipvars3 = lm(logPrice~Lot.Area + Overall.Qual + Overall.Cond +        
                    BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath  +  
                    Fireplaces + Garage.Yr.Blt + Open.Porch.SF +
                    Land.Slope+ Condition.1  + Bldg.Type + House.Style+
                    + Exter.Qual +Bsmt.Qual + BsmtFin.Type.2 + Kitchen.Qual+
                    Functional + Garage.Type+Sale.Type,
                  data = iowa, subset= train)
summary(test.fit_vipvars3)
plot(test.fit_vipvars3$residuals~test.fit_vipvars3$fitted.values)
hist(stdres(test.fit_vipvars3))
qqnorm(test.fit_vipvars3$residuals)
shapiro.test(test.fit_vipvars3$residuals)

#combine all the data together and test the model using entire data set
iowa_full$logPrice = log(iowa_full$SalePrice)
model = lm(logPrice~Lot.Area + Overall.Qual + Overall.Cond +        
             BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath  +  
             Fireplaces + Garage.Yr.Blt + Open.Porch.SF +
             Land.Slope+ Condition.1  + Bldg.Type + House.Style+
             + Exter.Qual +Bsmt.Qual + BsmtFin.Type.2 + Kitchen.Qual+
             Functional + Garage.Type+Sale.Type,
           data = iowa_full, na.action=na.exclude )
summary(model)
plot(model$residuals~model$fitted.values)
hist(stdres(model))
qqnorm(model$residuals)
shapiro.test(model$residuals)
#show and remove outliers
#add standard residual column to data set 
iowa_full$sres = stdres(model)
#show outliers using standard residual column 
subset(iowa_full,model$sres< -3)
subset(iowa_full,model$sres> 3)
#modify dataset to exclude outliers
iowa_noOut=subset(iowa_full,abs(iowa_full$sres) < 3)
#rerun dianogstics to see if problems are fixed
model.o = lm(logPrice~Lot.Area + Overall.Qual + Overall.Cond +        
             BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath  +  
             Fireplaces + Garage.Yr.Blt + Open.Porch.SF +
             Land.Slope+ Condition.1  + Bldg.Type + House.Style+
             + Exter.Qual +Bsmt.Qual + BsmtFin.Type.2 + Kitchen.Qual+
             Functional + Garage.Type+Sale.Type,
           data = iowa_noOut, na.action=na.exclude )
summary(model.o)
plot(model.o$residuals~model.o$fitted.values)
hist(stdres(model.o))
qqnorm(model.o$residuals)
shapiro.test(model.o$residuals)
#data still has outliers 
#redo the above process but using the new dataset
iowa_noOut$sres = stdres(model.o)
subset(iowa_noOut,model.o$sres< -3)
subset(iowa_noOut,model.o$sres> 3)
iowa_noOut1=subset(iowa_noOut,abs(iowa_noOut$sres) < 3)

model.o1 = lm(logPrice~Lot.Area + Overall.Qual + Overall.Cond +        
               BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath  +  
               Fireplaces + Garage.Yr.Blt + Open.Porch.SF +
               Land.Slope+ Condition.1  + Bldg.Type + House.Style+
               + Exter.Qual +Bsmt.Qual + BsmtFin.Type.2 + Kitchen.Qual+
               Functional + Garage.Type+Sale.Type,
             data = iowa_noOut1, na.action=na.exclude )
summary(model.o1)
plot(model.o1$residuals~model.o1$fitted.values)
hist(stdres(model.o1))
qqnorm(model.o1$residuals)
shapiro.test(model.o1$residuals)
iowa_noOut1$sres = stdres(model.o1)

#data still has outliers 
#redo the above process but using the new dataset
subset(iowa_noOut1,model.o1$sres < -3)
subset(iowa_noOut1,model.o1$sres >  3)
iowa_noOut2=subset(iowa_noOut1,abs(iowa_noOut1$sres) < 3)
model.o2 = lm(logPrice~Lot.Area + Overall.Qual + Overall.Cond +        
                BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath  +  
                Fireplaces + Garage.Yr.Blt + Open.Porch.SF +
                Land.Slope+ Condition.1  + Bldg.Type + House.Style+
                + Exter.Qual +Bsmt.Qual + BsmtFin.Type.2 + Kitchen.Qual+
                Functional + Garage.Type+Sale.Type,
              data = iowa_noOut2, na.action=na.exclude )
summary(model.o2)
plot(model.o2$residuals~model.o2$fitted.values)
hist(stdres(model.o2))
qqnorm(model.o2$residuals)
shapiro.test(model.o2$residuals)
iowa_noOut2$sres = stdres(model.o2)

#data still has outliers 
#redo the above process but using the new dataset
subset(iowa_noOut2,model.o2$sres < -3)
subset(iowa_noOut2,model.o2$sres >  3)
iowa_noOut3=subset(iowa_noOut2,abs(iowa_noOut2$sres) < 3)
model.o3 = lm(logPrice~Lot.Area + Overall.Qual + Overall.Cond +        
                BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath  +  
                Fireplaces + Garage.Yr.Blt + Open.Porch.SF +
                Land.Slope+ Condition.1  + Bldg.Type + House.Style+
                + Exter.Qual +Bsmt.Qual + BsmtFin.Type.2 + Kitchen.Qual+
                Functional + Garage.Type+Sale.Type,
              data = iowa_noOut3, na.action=na.exclude )
summary(model.o3)
plot(model.o3$residuals~model.o3$fitted.values)
hist(stdres(model.o3))
qqnorm(model.o3$residuals)
shapiro.test(model.o3$residuals)


##Final Model and prediction
predict = read.csv("AmesHousing_predict.csv")
model_final = lm(logPrice~Lot.Area + Overall.Qual + Overall.Cond +        
                   BsmtFin.SF.1 + Bsmt.Unf.SF + X1st.Flr.SF + Half.Bath  +  
                   Fireplaces + Garage.Yr.Blt + Open.Porch.SF +
                   Land.Slope+ Condition.1  + Bldg.Type + House.Style+
                   + Exter.Qual +Bsmt.Qual + BsmtFin.Type.2 + Kitchen.Qual+
                   Functional + Garage.Type+Sale.Type,
                 data = iowa_noOut3, na.action=na.exclude )
pred=predict(model_final,predict,interval="prediction")
pred


