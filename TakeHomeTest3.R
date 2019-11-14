#load in data and create test and training sets
weather = read.csv('weather.csv')
set.seed(1)
train=sample(17378,15000)
test=(c(1:17378)[-train])
wetrain=weather[train,]
wetest=weather[test,]

###############################################
###########  LOGISTIC MODEL ###################
###############################################
#fit model with only statistically significant variables
#started with all variables
fit=glm(RainTomorrow ~ . -WindGustDir -Temp3pm -Evaporation, family = binomial, data = wetrain)
summary(fit)

prob=predict(fit,newdata=wetrain,type="response")

library(ROCR)
pre=prediction(prob,wetrain$RainTomorrow)
perf = performance(pre,"tpr","fpr")
plot(perf,main="ROC plot")
perf=performance(pre,"fpr","cutoff")
plot(perf)
perf=performance(pre,"fnr","cutoff")
plot(perf)
perf=performance(pre,"tpr","rpp")
plot(perf, main="gain curve",print.cutoffs.at=c(.1,.2,.3,.4,.5))
abline(0,1)

#cutoff = 0.5
pred=ifelse(prob>.5,1,0)
table( pred, wetrain$RainTomorrow)

#total error
(707+1542)/(707+1542+10854+1897)
#false pos
(707)/(707+10854)
#false neg
(1542)/(1542+1897)

#errors for testing data
#cutoff = 0.5
test_prob=predict(fit,newdata=wetest,type="response")
pred=ifelse(test_prob>.5,1,0)
table( pred, wetest$RainTomorrow)

#total error
(117+226)/(117+1748+226+287)
#false pos
(117)/(117+1748)
#false neg
(226)/(226+287)


###############################################
###########  CLASS TREE MODEL #################
###############################################
library(rpart)
library(rpart.plot)
#  estimate the tree and print out the results
m1=rpart(RainTomorrow~.,data=wetrain,method="class")
printcp(m1)
plotcp(m1)
rpart.plot(m1,type=3,extra=1)

#prune the tree based on cp value
m2=prune(m1,cp=.018465)
rpart.plot(m2,type = 3,extra=1)
#  Plot the ROC type plots
library(ROCR)
prob=predict(m2,wetrain,type="prob")
pred=prediction(prob[,2],wetrain$RainTomorrow)
perf = performance(pred ,"tpr","fpr")
plot(perf,main="ROC plot")
perf=performance(pred,"fpr","cutoff")
plot(perf)
perf=performance(pred,"fnr","cutoff")
plot(perf)
#  Computing and printing the confusion matrix for the training data with a cutoff of .5
# If you want to change the cutoff you need to change the value .5 in the command
prob=predict(m2,newdata=wetrain,type="prob")
pred=ifelse(prob[,2]>.4,1,0)
table( pred,wetrain$RainTomorrow)

error_print(11300, 261, 2362, 1076)


#  Computing and printing the confusion matrix for the test data with a cutoff of .5
prob=predict(m2,newdata=wetest,type="prob")
pred=ifelse(prob[,2]>.4,1,0)
table( pred,wetest$RainTomorrow)
error_print(1825, 40, 374, 139)

###############################################
###########  NAIVE BAYES MODEL ################
###############################################

#  Plot histograms for the continuous data for the two groups in the response variable.
library(lattice)
histogram(~MinTemp|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~MaxTemp|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~Rainfall|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~Evaporation|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~Sunshine|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~WindGustSpeed|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~WindSpeed3pm|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~Pressure3pm|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~Cloud3pm|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~Temp3pm|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~AveHumidity|RainTomorrow,data=wetrain,layout=c(1,2))
histogram(~DifHumidity|RainTomorrow,data=wetrain,layout=c(1,2))

#  Perform a test of independence for the categorical variables.
chisq.test(wetrain$RainTomorrow,wetrain$WindGustDir)
chisq.test(wetrain$RainTomorrow,wetrain$WindDir3pm)
chisq.test(wetrain$RainTomorrow,wetrain$RainToday)

#  loantrain is the name of the dataframe containing the training data  loantrain$Response is the name of the #  response variable.
# get print
columnlist(wetrain)
x.vars= c(5, 6, 8, 10, 11, 13, 15)
x=wetrain[,x.vars]
y=wetrain$RainTomorrow
library(naivebayes)
model=naive_bayes(x,y,usekernel=TRUE)
plot(model)
#  ROC and other plots
#  model is the name of the Naïve Bayes estimated model and x in the input variables
library(ROCR)
prob=predict(model,x, type = "prob")
pred=prediction(prob[,2],y)
perf = performance(pred ,"tpr","fpr")
plot(perf,main="ROC plot")
perf=performance(pred,"fpr","cutoff")
plot(perf)
perf=performance(pred,"fnr","cutoff")
plot(perf)
#  plot the gain curve
perf=performance(pre,"tpr","rpp")
plot(perf, main="gain curve",print.cutoffs.at=c(.1,.2,.3,.4,.5))
abline(0,1)
#  Computing and printing the confusion matrix
#  model is the name of the Naïve Bayes estimated model and x is the input variables
#  y is the name of the response variable and .3 is the cutoff value
prob=predict(model,x, type = "prob")
pred=ifelse(prob[,2]>.2,1,0)
table( pred,y)
error_print(7705,3856,448,2991)
#  Modifications to compute and print the confusion matrix for the test data
#  Note that x and y need to be reset to the test data values.
x=wetest[,x.vars]
y=wetest$RainTomorrow
prob=predict(model,x, type = "prob")
pred=ifelse(prob[,2]>.4,1,0)
table( pred,y)
error_print(1408, 457, 104, 409)




###############################################
###########  NEURAL NET MODEL #################
###############################################

minmax = function(x){
  x=(x-min(x))/(max(x)-min(x))
  return(x)
}
weather$MinTemp_mm=minmax(weather$MinTemp)
weather$MaxTemp_mm=minmax(weather$MaxTemp)
weather$Rainfall_mm=minmax(weather$Rainfall)
weather$Evaporation_mm=minmax(weather$Evaporation)
weather$Sunshine_mm=minmax(weather$Sunshine)
weather$WindGustSpeed_mm=minmax(weather$WindGustSpeed)
weather$WindGustSpeed3pm_mm=minmax(weather$WindGustSpeed3pm)
weather$Pressure3pm_mm=minmax(weather$Pressure3pm)
weather$Cloud3pm_mm=minmax(weather$Cloud3pm)
weather$Temp3pm_mm=minmax(weather$Temp3pm)
weather$AveHumidity_mm=minmax(weather$AveHumidity)
weather$DifHumidity_mm=minmax(weather$DifHumidity)
# Example of creating an indicator variable for a categorical variable.  If the categorical variable has k
#  values you need k-1 indicator variables.
#windGustDir, WindDir3pm
weather$WindGustDir_1[weather$WindGustDir==1]=1
weather$WindGustDir_1[weather$WindGustDir!=1]=0
weather$WindGustDir_2[weather$WindGustDir==2]=1
weather$WindGustDir_2[weather$WindGustDir!=2]=0
weather$WindGustDir_3[weather$WindGustDir==3]=1
weather$WindGustDir_3[weather$WindGustDir!=3]=0
weather$WindGustDir_4[weather$WindGustDir==4]=1
weather$WindGustDir_4[weather$WindGustDir!=4]=0
weather$WindGustDir_5[weather$WindGustDir==5]=1
weather$WindGustDir_5[weather$WindGustDir!=5]=0
weather$WindGustDir_6[weather$WindGustDir==6]=1
weather$WindGustDir_6[weather$WindGustDir!=6]=0
weather$WindGustDir_7[weather$WindGustDir==7]=1
weather$WindGustDir_7[weather$WindGustDir!=7]=0
weather$WindGustDir_8[weather$WindGustDir==8]=1
weather$WindGustDir_8[weather$WindGustDir!=8]=0
weather$WindGustDir_9[weather$WindGustDir==9]=1
weather$WindGustDir_9[weather$WindGustDir!=9]=0
weather$WindGustDir_10[weather$WindGustDir==10]=1
weather$WindGustDir_10[weather$WindGustDir!=10]=0
weather$WindGustDir_11[weather$WindGustDir==11]=1
weather$WindGustDir_11[weather$WindGustDir!=11]=0
weather$WindGustDir_12[weather$WindGustDir==12]=1
weather$WindGustDir_12[weather$WindGustDir!=12]=0
weather$WindGustDir_13[weather$WindGustDir==13]=1
weather$WindGustDir_13[weather$WindGustDir!=13]=0
weather$WindGustDir_14[weather$WindGustDir==14]=1
weather$WindGustDir_14[weather$WindGustDir!=14]=0
weather$WindGustDir_15[weather$WindGustDir==15]=1
weather$WindGustDir_15[weather$WindGustDir!=15]=0

weather$WindDir3pm_1[weather$WindDir3pm==1]=1
weather$WindDir3pm_1[weather$WindDir3pm!=1]=0
weather$WindDir3pm_2[weather$WindDir3pm==2]=1
weather$WindDir3pm_2[weather$WindDir3pm!=2]=0
weather$WindDir3pm_3[weather$WindDir3pm==3]=1
weather$WindDir3pm_3[weather$WindDir3pm!=3]=0
weather$WindDir3pm_4[weather$WindDir3pm==4]=1
weather$WindDir3pm_4[weather$WindDir3pm!=4]=0
weather$WindDir3pm_5[weather$WindDir3pm==5]=1
weather$WindDir3pm_5[weather$WindDir3pm!=5]=0
weather$WindDir3pm_6[weather$WindDir3pm==6]=1
weather$WindDir3pm_6[weather$WindDir3pm!=6]=0
weather$WindDir3pm_7[weather$WindDir3pm==7]=1
weather$WindDir3pm_7[weather$WindDir3pm!=7]=0
weather$WindDir3pm_8[weather$WindDir3pm==8]=1
weather$WindDir3pm_8[weather$WindDir3pm!=8]=0
weather$WindDir3pm_9[weather$WindDir3pm==9]=1
weather$WindDir3pm_9[weather$WindDir3pm!=9]=0
weather$WindDir3pm_10[weather$WindDir3pm==10]=1
weather$WindDir3pm_10[weather$WindDir3pm!=10]=0
weather$WindDir3pm_11[weather$WindDir3pm==11]=1
weather$WindDir3pm_11[weather$WindDir3pm!=11]=0
weather$WindDir3pm_12[weather$WindDir3pm==12]=1
weather$WindDir3pm_12[weather$WindDir3pm!=12]=0
weather$WindDir3pm_13[weather$WindDir3pm==13]=1
weather$WindDir3pm_13[weather$WindDir3pm!=13]=0
weather$WindDir3pm_14[weather$WindDir3pm==14]=1
weather$WindDir3pm_14[weather$WindDir3pm!=14]=0
weather$WindDir3pm_15[weather$WindDir3pm==15]=1
weather$WindDir3pm_15[weather$WindDir3pm!=15]=0

column


# Create a new data.frame with only the 1/0 and min/max data
#  -c(1,2,3,4,5,6,7) are the variables to exclude from the old dataframe named credit.  The new dataframe name # is credit.newdat, cretrain is the training data set and cretest is the test data set.

weather.newdat=as.data.frame(weather[,-c(1,2,3,4,5,6,7,8,9,10,11,12,15,16)])
# Next create the training set and the test sets by executing the following R commands.
#wetrain=weather[train,]
#wetest=weather[test,]
wetrain=weather.newdat[train,]
wetest=weather.newdat[test,]
#  estimating the neural net model
#  Response is the response variable, loantrain is the input data set, size = 5 specifies 5 hidden layers.
#  maxit is the maximum number of iterations
#  the nnet command should be executed more than one time since you get different results each time.  
#  You want to pick the lowest final value
library(nnet)
net1=nnet(RainTomorrow ~ .,data=wetrain,size=5,maxit=10000)
net2=nnet(RainTomorrow ~ .,data=wetrain,size=5,maxit=10000)
net3=nnet(RainTomorrow ~ .,data=wetrain,size=5,maxit=10000)
net4=nnet(RainTomorrow ~ .,data=wetrain,size=5,maxit=10000)
net5=nnet(RainTomorrow ~ .,data=wetrain,size=5,maxit=10000)
net6=nnet(RainTomorrow ~ .,data=wetrain,size=5,maxit=10000)
net7=nnet(RainTomorrow ~ .,data=wetrain,size=5,maxit=10000)
net8=nnet(RainTomorrow ~ .,data=wetrain,size=5,maxit=10000)
net9=nnet(RainTomorrow ~ .,data=wetrain,size=5,maxit=10000)
net10=nnet(RainTomorrow ~ .,data=wetrain,size=5,maxit=10000)
library(NeuralNetTools)
gar=garson(net9)
win.graph(width=8.875, height = 3.5,pointsize=8)
plot(gar)
#  Ploting ROC type plots
#  net10 is the name of the estimated neural net, cretrain is the dataframe containing the input data.
library(ROCR)
prob = predict(net10,wetrain)
pred=prediction(prob,wetrain$RainTomorrow)
perf = performance(pred ,"tpr","fpr")
plot(perf,main="ROC plot")
perf=performance(pred,"fpr","cutoff")
plot(perf)
perf=performance(pred,"fnr","cutoff")
plot(perf)

#  Computing and printing the confusion matrix
#  net10 is the name of the estimated neural net, loantrain is the dataframe containing the input data.
#  .3 is the cutoff value and loantrain$Response is the response variable in the input set loantrain
prob = predict(net10,wetrain)
pred=ifelse(prob>.3,1,0)
table(pred,wetrain$RainTomorrow)
error_print(9918, 1643,884, 2555)

prob = predict(net10,wetest)
pred=ifelse(prob>.3,1,0)
table(pred,wetest$RainTomorrow)
error_print(1587, 278,140, 373)

#Helper function to list vars in table with column index
columnlist=function(x){
  count = 1
  for (i in names(x)) {
    print(cat(count,i))
    count = count + 1
  }
}
columnlist(weather)

error_print=function(x1, x2, y1, y2){
  message("total error: ",(x2+y1)/(x1+x2+y1+y2))
  message("false positive error: ",(x2)/(x1+x2))
  message("false negative error: ",(y1)/(y1+y2))
}
