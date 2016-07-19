#install.packages("alr3")
library(alr3)
data(snake)
attach(snake)
dim(snake)
head(snake)


names(snake) = c("content", "yield") #changing X and Y into meaningful variable names
attach(snake) #reattach data with new names
head(snake)
plot(content, yield, xlab="water content of snow", ylab="water yield")
#Data is linear

#Fitting linear model 
yield.fit = lm(yield~content)
summary(yield.fit)
#P-value is highly significant 
#87 percent of the variation in the water yield can be explained by the water content of snow

plot(content, yield)
abline(yield.fit, lwd=3, col="red")

#We will be dealing with only linearity and heteroscedasticity, as we are building a univariate model not dependent on "time"
#we will not touch, coorelation of errors, collinearity and presence of outliers. 

par(mfrow=c(2,2))
plot(yield.fit)


par(mfrow=c(1,1))
qqPlot(yield.fit)
#According to the plot, the residuals are normally distributed.this can give us some confidence to select the model 
#with all the observations 

