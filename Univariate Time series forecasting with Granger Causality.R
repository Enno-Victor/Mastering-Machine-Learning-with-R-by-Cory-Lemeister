url1 = "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.4.0.0.annual_ns_avg.txt"
temp = read.fwf(url1, widths=c(4,3,6),sep="")
head(temp)
tail(temp)
str(temp)
temp = temp[-c(167), ]
str(temp)

names(temp)=c("Year","Temperature")
#Converting in time series 
T = ts(temp$Temperature, frequency=1, start=c(1850), end=c(2015))

url2 = "http://cdiac.ornl.gov/ftp/ndp030/CSV-FILES/global.1751_2011.csv"
co2 = read.csv(file=url2,skip=2,header=FALSE, col.names=c("Year","Total","3","4","5","6","7","8"))
str(co2)
co2 = co2[,1:2]
head(co2)
tail(co2)
#Converting in time series
E = ts(co2$Total, frequency=1, start=c(1751),end=c(2011))


##Data understanding and preparation
#install.packages("dynlm")
library(dynlm)
#install.packages("forecast")
library(forecast)
#install.packages("tseries")
library(tseries)
#install.packages("vars")
library(vars)

SurfaceTemp = window(T, start=c(1920), end=c(2011))
Emissions = window(E, start=c(1920), end=c(2011))

# combining both the time series to one object
climate = cbind(SurfaceTemp, Emissions)
plot(climate, main="Temp Anomalies and CO2 Emissions")
#plot shows that the temperature anomalies started to increase from the norm roughly around 1970. Emissions seem 
#to begin a slow uptick in the mid-40s and a possible trend increase in 2000. 

#two series are highly correlated
cor(climate)

#plotting ACF and PACF for both the series on the same plot
par(mfrow=c(2,2))
acf(climate[,1], main="Temp")
pacf(climate[,1], main="Temp")
acf(climate[,2], main="CO2")
pacf(climate[,2], main="CO2")
#With the decaying ACF patterns and rapidly decaying PACF patterns, it could be assumed that these series are both autoregressive 

#Cross Correlation Function (CCF). 
par(mfrow=c(1,1))
ccf(climate[,1],climate[,2], main="CCF")

#Additionally, to a calibrated eye, the data is not stationary;   Augmented Dickey-Fuller (ADF) test 
adf.test(climate[,1])
adf.test(climate[,2])
#the p-values are not significant, so we fail to reject the null hypothesis of the test that the data is not stationary

##Univariate time series forecasting
T2 = window(T, start=1970)
plot(T2)

train = window(T2,end=2007)
test = window(T2,start=2008)

#Using holt method for smoothing 
library(forecast)
fit.holt=holt(train, h=8, initial="optimal")
summary(fit.holt)
plot(forecast(fit.holt))
lines(test, type="o")

#forecast overshot the mark a little bit. Let's have a go by including the damped trend
fit.holtd =holt(train, h=8, initial="optimal", damped=TRUE)
summary(fit.holtd)
plot(forecast(fit.holtd), main = "Holt Damped")
lines(test, type="o")

#ARIMA model
fit.arima = auto.arima(train)
summary(fit.arima)
plot(forecast(fit.arima, h=8))
lines(test, type="o")

#examining MAPE on the test set
mape1 = sum(abs((test-fit.holtd$mean)/test))/8
mape1
mape2 = sum(abs((test-forecast(fit.arima)$mean)/test))/8
mape2
#The forecast error is more for the Holt Damped trend model versus ARIMA(2,1,0)
###With the statistical and visual evidence, it seems that the best choice for a univariate forecast model is the 
#ARIMA (2,1,0)

#plot with all the three forecasts
T3=window(T2, start=1990)
plot(T3, ylim=c(0.1,0.8))
lines(forecast(fit.holt)$mean, type="o",pch=2,lty="dotted")
lines(forecast(fit.holtd)$mean, type="o",pch=5,lty=6)
lines(forecast(fit.arima,h=8)$mean, type="o",pch=7,lty="dashed")
legend("topleft", lty=c("solid","dotted","dashed"), pch=c(1,2,5,7), c("Data","Holt","HoltDamped","ARIMA"))
#completed the building of a univariate forecast model for the surface temperature anomalies and now we will move on to the next task

##Time series regression
#we will apply the techniques to the climate change data
y = window(climate[,1],start=1945)
x = window(climate[,2],start=1945)

fit.lm = lm(y~x)
summary(fit.lm)
#F-statistic for the overall model is highly statistically significant (<2.2e-16) and that the x variable (CO2) is also 
#highly significant. Adjusted R-squared is 0.6435

plot.ts(fit.lm$residuals)
acf(fit.lm$residuals)

# Durbin-Watson test. This tests the null hypothesis that the residuals have zero autocorrelation.
dwtest(fit.lm)

#cross correlation structure
ccf(x,y)

#let's start by looking at six lags of x and lag-1 and lag-4 of y
fit.dyn = dynlm(y~x+L(x,1:6)+L(y,c(1,4)))
summary(fit.dyn)
#Adjusted R-squared of 0.8091

#We can adjust the model by dropping the insignificant x lags and then test the assumption of no serial correlation
fit.dyn2 = dynlm(y~L(x,c(5,6))+L(y,c(1,4)))
summary(fit.dyn2)
#Adjusted R-squared has improved slightly

plot(fit.dyn2$residuals)
acf(fit.dyn2$residuals)

dwtest(fit.dyn2)

#plotting the Actual versus Predicted values 
plot(y, ylab="Surface Temperature")
lines(fitted(fit.dyn2), pch=2, lty="dashed")
legend("topleft", lty=c("solid","dashed"), c("Actual","Predicted"))

##Examining the causality
ndiffs(x, test="kpss")
ndiffs(y, test="kpss")

Granger = cbind(y,x)
dGranger = diff(Granger)

lag=VARselect(dGranger, lag.max=10)

lag$selection

lag5 = VAR(dGranger, p=5)
summary(lag5)

serial.test(lag5,type="PT.asymptotic")
#With p-value at 0.7839, we do not have evidence to reject the null and can say that the residuals are not autocorrelated

#Granger test 
x2y = causality(lag5,cause="x")
y2x = causality(lag5,cause="y")

x2y$Granger
y2x$Granger
#we cannot reject the null at the 0.05 significance level and therefore, conclude that x does not Granger-cause y

## vector autoregression in order to produce a forecast
predict(lag5, n.ahead=10, ci=0.95)
plot(forecast(lag5))



