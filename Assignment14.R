#read in the data source
Donner = read.csv("Donner.csv")

#plot a boxplot as a preliminary for a continuous predictor
boxplot(Donner$Age~Donner$Surv, ylab = "Age", xlab = "Survived")

#create a cross tabulation and do a chisquare test of null hypothesis
xtabs(~ Gender+Surv, data=Donner)
chisq.test(Donner$Gender,Donner$Surv)

#fit a logistic regression model to the data w variables age, gender, multfam v. surv
fit = glm(formula = Surv ~ Age + Gender + MultFam, family = binomial, data = Donner)
summary(fit)

#Remove Gender since it is not significant
fit1 = glm(formula = Surv ~ Age + MultFam, family = binomial, data = Donner)
summary(fit1)

#try fitting a more complex model to check the fit of the first model
fit_comx = glm(formula = Surv~Age + MultFam + Age*MultFam, family = binomial, data = Donner)
summary(fit_comx)
#do anova command to compare the two models
anova(fit1, fit_comx, test = "Chisq")

#checking the model by preforming the Hosmer-Lemeshow goodness of fit
library(ResourceSelection)
hoslem.test(fit1$y, fitted(fit1), g=10)
hoslem.test(fit_comx$y, fitted(fit_comx), g=10)

###############################################################
#############Question/Data Set 2 ###############################
##############################################################

#read in the data
loan=read.csv("LoanAccept.csv")
set.seed(1)
#create a training set and testing set
train=sample(5000,4000)
test=(c(1:5000)[-train])
loan.train=loan[train,]

#stepwise procedure to identify which variables to use
null=glm(Response~1,family=binomial,data=loan.train)
full=glm(Response~.-Educ+factor(Educ), family=binomial,data=loan.train)
step(null,scope=list(lower=null,upper=full),direction="both")

#fit model with stepwise relavent variables
fit = glm(formula = Response~ Inc+factor(Educ)+CD+Fam+Online+CreditCard
          +CCAve+SecAcc, family = binomial, data = loan, subset = train)
summary(fit)

#fit model using testing data
fit.test = glm(formula = Response~ Inc+factor(Educ)+CD+Fam+Online+CreditCard
          +CCAve+SecAcc, family = binomial, data = loan, subset = test)
summary(fit.test)
