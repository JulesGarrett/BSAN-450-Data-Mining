loan=read.csv("LoanAccept.csv")
set.seed(1)
train=sample(5000,4000)
test=(c(1:5000)[-train])


# change the variable Response to a factor, the Y must be a factor in the knn function.
loan$Response=factor(loan$Response)
# scale the variables that are on a continuous scale
num.vars = c(1,2,3,4,5,7)
loan[num.vars] = lapply(loan[num.vars], scale)
# create 3 variables for the categorical variable  Educ.  
loan$e1 =0
loan$e2 =0
loan$e3 = 0
loan$e1[loan$Educ ==1] = 1/sqrt(2)
loan$e2[loan$Educ==2] = 1/sqrt(2)
loan$e3[loan$Educ==3] = 1/sqrt(2)


#  select the variables that will be used as the inputs 
x.vars= c(1,2,3,4,5,7,9,10,11,12,13,14,15)
x=loan[,x.vars]
#  create the training and test sets
xtrain =x[train,]
xtest = x[test,]
ytrain = loan$Response[train]
ytest = loan$Response[test]


library(class)
#  use leave one out cross validation to determine the value of K.  o.error is the overall error, fp.rate is 
# the false positive error and fn.rate is the false negative error.  Try k = 1 to 10
set.seed(1)
o.error=rep(NA,10)
fp.rate=rep(NA,10)
fn.rate=rep(NA,10)
for(i in 1:10){
  knn.pred=knn.cv(xtrain,ytrain,k=i)
  ttt =table(knn.pred,ytrain)
  o.error[i]=(ttt[1,2]+ttt[2,1])/(ttt[1,1]+ttt[1,2]+ttt[2,1]+ttt[2,2])
  fp.rate[i]= (ttt[2,1])/(ttt[2,1]+ttt[1,1])
  fn.rate[i]=(ttt[1,2])/(ttt[1,2]+ttt[2,2])
}
o.error
fp.rate
fn.rate
#  compute the confusion matrix for the test data
knn.pred=knn(xtrain,xtest,ytrain,k=3)
table(knn.pred,ytest)


#read the data in
glow=read.csv("glow500.csv")
set.seed(1)
train=sample(500,400)
test=(c(1:500)[-train])


# change the variable Response to a factor, the Y must be a factor in the knn function.
glow$FRACTURE=factor(glow$FRACTURE)

# scale the variables that are on a continuous scale
glow$AGE = (glow$AGE - mean(glow$AGE))/sd(glow$AGE)
glow$WEIGHT = (glow$WEIGHT - mean(glow$WEIGHT))/sd(glow$WEIGHT)
glow$HEIGHT = (glow$HEIGHT - mean(glow$HEIGHT))/sd(glow$HEIGHT)

#expand RATERISK bc has 3 categories. 
glow$r1 =0
glow$r2 =0
glow$r3 = 0
glow$r1[glow$RATERISK ==1] = 1/sqrt(2)
glow$r2[glow$RATERISK==2] = 1/sqrt(2)
glow$r3[glow$RATERISK==3] = 1/sqrt(2)


#  select the variables that will be used as the inputs 
x.vars= c(1,2,3,4,5,7,9,10,11,12,13)
x=glow[,x.vars]
#  create the training and test sets
xtrain =x[train,]
xtest = x[test,]
ytrain = glow$FRACTURE[train]
ytest = glow$FRACTURE[test]


library(class)
#  use leave one out cross validation to determine the value of K.  o.error is the overall error, fp.rate is 
# the false positive error and fn.rate is the false negative error.  Try k = 1 to 10
set.seed(1)
o.error=rep(NA,10)
fp.rate=rep(NA,10)
fn.rate=rep(NA,10)
for(i in 1:10){
  knn.pred=knn.cv(xtrain,ytrain,k=i)
  ttt =table(knn.pred,ytrain)
  o.error[i]=(ttt[1,2]+ttt[2,1])/(ttt[1,1]+ttt[1,2]+ttt[2,1]+ttt[2,2])
  fp.rate[i]= (ttt[2,1])/(ttt[2,1]+ttt[1,1])
  fn.rate[i]=(ttt[1,2])/(ttt[1,2]+ttt[2,2])
}
o.error
fp.rate
fn.rate
#  compute the confusion matrix for the test data
knn.pred=knn(xtrain,xtest,ytrain,k=3)
table(knn.pred,ytest)
