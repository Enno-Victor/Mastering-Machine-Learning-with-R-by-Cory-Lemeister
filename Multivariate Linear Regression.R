data(water)
str(water)

socal.water = water[ ,-1] #new dataframe with the deletion of column 1;we are not concerned with what year the observations occurred
head(socal.water)
#BSAAM is response variable

#install.packages("corrplot")
library(corrplot)
water.cor = cor(socal.water)
water.cor
corrplot(water.cor, method="ellipse")
#the response variable (BSAAM) is highly and positively correlated with the OP features with OPBPC as 0.8857, OPRC as 0.9196, 
#and OPSLAKE as 0.9384; case of MULTICOLLINEARITY


#install.packages("leaps")
library(leaps)
fit=lm(BSAAM~., data=socal.water)
summary(fit)
#p-values for OPRC and OPSLAKE parameters are statistically significant, but p-value OPBPC parameter is not statistically significant 
#which is to say that the feature OPBPC adds nothing from a statistical standpoint with OPRC and OPSLAKE in the model

#Identifying best subsets 
sub.fit = regsubsets(BSAAM~., data=socal.water)
best.summary = summary(sub.fit)
which.min(best.summary$rss)

#Analyzing Cp, BIC and R_Squared
par(mfrow=c(1,2))
plot(best.summary$cp, xlab="number of features", ylab="cp")
plot(sub.fit, scale="Cp")
#APSLAKE, OPRC, and OPSLAKE are selected features from Cp method

which.min(best.summary$bic)
which.max(best.summary$adjr2)
#three features selected, same number as Cp 

#Based on Cp, BIC and R-Squared measures fitting the linear model 
best.fit = lm(BSAAM~APSLAKE+OPRC+OPSLAKE, data=socal.water)
summary(best.fit)
#All three features' p-value is statistically significant 

#Producing diagnostic plots 
par(mfrow=c(2,2))
plot(best.fit)

#Looking at the plots, it seems safe to assume that the residuals have a constant variance and are normally distributed. 
#There is nothing in the leverage plot that would indicate a requirement for further investigation

#Checking collinearity using Variance Inflation Factor (VIF) statistic
vif(best.fit)
#Potential collinearity problem with OPRC  and  OPSLAKE (values greater than five)
par(mfrow=c(1,1))
plot(socal.water$OPRC, socal.water$OPSLAKE, xlab="OPRC", ylab="OPSLAKE")
best.summary$adjr2 #adjusted r-squared values
#the two-variable model of APSLAKE and OPSLAKE produced a value of 0.90, while adding OPRC only marginally increased it to 0.92

#two-variable model
fit.2 = lm(BSAAM~APSLAKE+OPSLAKE, data=socal.water)
summary(fit.2)
par(mfrow=c(2,2))
plot(fit.2)
vif(fit.2)
#Values are less than 5 and collinearity problem is been taken care of

#Checking comstant variance of errors 
install.packages("lmtest")
library(lmtest)
bptest(fit.2)
#We donot have evidence to reject the null that implies the error variances are zero because p-value = 0.9977

#The model can explain 90 percent of the variation in the stream runoff volume
par(mfrow=c(1,1))
plot(fit.2$fitted.values, socal.water$BSAAM,xlab="predicted", ylab="actual", main="Predicted vs.Actual")


#Improving graphics
socal.water["Actual"] = water$BSAAM 
socal.water["Forecast"] = NA 
socal.water$Forecast = predict(fit.2)
library(ggplot2)
ggplot(socal.water, aes(x=Forecast, y=Actual)) +geom_point() + geom_smooth(method=lm) + labs(title = "Forecast versus Actuals")

#Performing LOOCV 
#install.packages("MPV")
library(MPV)
PRESS(best.fit)
PRESS(fit.2)
#Although the measure says best.fit is better model, but we will go with more parsimonious model (fit.2)


