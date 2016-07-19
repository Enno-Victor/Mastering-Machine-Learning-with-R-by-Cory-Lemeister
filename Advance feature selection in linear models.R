##Data understanding and preperation 
library(ElemStatLearn) #contains the data
library(car) #package to calculate Variance Inflation Factor
library(corrplot) #correlation plots
library(leaps) #best subsets regression
#install.packages("glmnet")
library(glmnet) #allows ridge regression, LASSO and elastic net
library(caret) #parameter tuning

data(prostate)
str(prostate)

plot(prostate)

#Inquiring gleason
plot(prostate$gleason)
table(prostate$gleason)
boxplot(prostate$lpsa~prostate$gleason, xlab="Gleason Score", ylab="Log of PSA")
#Turning gleason into an indicator variable 
prostate$gleason = ifelse(prostate$gleason == 6, 0, 1)
table(prostate$gleason)

#Correlation plot 
p.cor = cor(prostate)
corrplot.mixed(p.cor)
#PSA is highly correlated with the log of cancer volume (lcavol)
#multicollinearity may become an issue; for example, cancer volume is also correlated with capsular penetration and 
#this is correlated with the seminal vesicle invasion

#Creating training and test datasets
train = subset(prostate, train==TRUE)[,1:9]
str(train)
test = subset(prostate, train==FALSE)[,1:9]
str(test)

## 1) Best subsets
subfit = regsubsets(lpsa~., data=train)
b.sum = summary(subfit)
which.min(b.sum$bic)
#3 features has the lowest bic value

plot(b.sum$bic, type="l", xlab="# of Features", ylab="BIC",  main="BIC score by Feature Inclusion")

plot(subfit, scale="bic", main="Best Subset Features")
#lcavol, lweight, and gleason features are selected

#constancy of the variance
ols = lm(lpsa~lcavol+lweight+gleason, data=train) #creating an object ols
plot(ols$fitted.values, train$lpsa, xlab="Predicted",  ylab="Actual", main="Predicted vs Actual")
#linear fit should perform well on this data and that the nonconstant variance is not a problem

#Trying model on the test portion of the data
pred.subfit = predict(ols, newdata=test)
plot(pred.subfit, test$lpsa , xlab="Predicted", ylab="Actual", main="Predicted vs Actual")
#The plot doesn't seem to be too terrible

#calculate mean squared error (MSE)
resid.subfit = test$lpsa - pred.subfit
mean(resid.subfit^2)

#MSE of 0.508
 
## RIDGE REGRESSION 
x = as.matrix(train[,1:8])
y = train[ ,9]
#glmnet(x = our input matrix, y = our response, family = the distribution, alpha=0). 
ridge = glmnet(x, y, family="gaussian", alpha=0)
print(ridge)


plot(ridge, label=TRUE)
plot(ridge, xvar="lambda", label=TRUE)
#as lambda decreases, the coefficients increase 

ridge.coef = coef(ridge, s=0.1, exact=TRUE) #lamba .1
ridge.coef
#age, lcp, and pgg45 are close to, but not quite, zero

plot(ridge, xvar="dev", label=TRUE)
#lambda decreases, /fraction of the deviance increases

#Trying model on test dataset
newx = as.matrix(test[,1:8])
ridge.y = predict(ridge, newx=newx, type="response", s=0.1)
plot(ridge.y, test$lpsa, xlab="Predicted", ylab="Actual",main="Ridge Regression")

ridge.resid = ridge.y - test$lpsa
mean(ridge.resid^2)

#Ridge regression has given us a slightly better MSE .4789913

##LASSO
lasso = glmnet(x, y, family="gaussian", alpha=1)
print(lasso)
#all the eight features should be in the model with a lambda of 0.001572

plot(lasso, xvar="lambda", label=TRUE)

#finding and testing a model with fewer features, around seven, for argument's sake
lasso.coef = coef(lasso, s=0.045, exact=TRUE)
lasso.coef
#The LASSO algorithm zeroed out the coefficient for lcp at a lambda of 0.045

lasso.y = predict(lasso, newx=newx, type="response", s=0.045)
plot(lasso.y, test$lpsa, xlab="Predicted", ylab="Actual", main="LASSO")

#calculating MSE
lasso.resid = lasso.y - test$lpsa
mean(lasso.resid^2)
#Similar plots as before with only the slightest improvement in MSE 0.4437209

## ELASTIC NET 
grid = expand.grid(.alpha=seq(0,1, by=.2), .lambda=seq(0.00,0.2, by=0.02))
table(grid)

control = trainControl(method="LOOCV")
enet.train = train(lpsa~., data=train, method="glmnet", trControl=control, tuneGrid=grid)
enet.train
#Using RMSE values,  final values are alpha = 0 and lamba = .08

#test set validation
enet = glmnet(x, y,family="gaussian", alpha=0, lambda=.08)
enet.coef = coef(enet, s=.08, exact=TRUE)
enet.coef
enet.y = predict(enet, newx=newx, type="response", s=.08)
plot(enet.y, test$lpsa, xlab="Predicted", ylab="Actual", main="Elastic Net")
enet.resid = enet.y - test$lpsa
mean(enet.resid^2)

#This model error is similar to the ridge penalty. On the test set, our LASSO model did the best in terms of errors. 
#We may be over-fitting! Our best subset model with three features is the easiest to explain, and in terms of errors, 
#is acceptable to the other techniques. We can use a 10-fold cross-validation in the glmnet package to possibly identify
#a better solution

#LASSO with CV
set.seed(317)
lasso.cv = cv.glmnet(x, y)
plot(lasso.cv)

lasso.cv$lambda.min #minimum
lasso.cv$lambda.1se #one standard error away

coef(lasso.cv, s ="lambda.1se")
lasso.y.cv = predict(lasso.cv, newx=newx, type="response", s="lambda.1se")
lasso.cv.resid = lasso.y.cv - test$lpsa
mean(lasso.cv.resid^2)

##This model achieves an error of 0.46 with just five features, zeroing out age, lcp, and pgg45

#•Best subsets is 0.51
#•Ridge regression is 0.48
#•LASSO is 0.44
#•Elastic net is 0.48
#•LASSO with CV is 0.46

#Book Author Notes 
#On a pure error, LASSO with seven features performed the best. However, does this best address the question that we 
#are trying to answer? Perhaps the more parsimonious model that we found using CV with a lambda of ~0.165 is more appropriate.
#My inclination is to put forth the latter as it is more interpretable.

