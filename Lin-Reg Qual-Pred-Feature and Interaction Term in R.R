#Qualitatve Feature 
#install.packages("ISLR")
library(ISLR)
data(Carseats)
str(Carseats)
sales.fit = lm(Sales~Advertising+ShelveLoc, data=Carseats)
contrasts(Carseats$ShelveLoc)

#Interaction term 
library(MASS)
data(Boston)
str(Boston)
value.fit = lm(medv~lstat*age, data=Boston)
