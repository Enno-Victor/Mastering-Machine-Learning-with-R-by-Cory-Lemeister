#Data understanding and preperation 

library(MASS)
data(biopsy)
str(biopsy)

biopsy$ID = NULL
names(biopsy) = c("thick", "u.size", "u.shape", "adhsn", "s.size", "nucl", "chrom", "n.nuc", "mit", "class")
names(biopsy)
View(biopsy)
#Checking for missing values
table(biopsy$nucl, useNA = "ifany")
biopsy.v2 = na.omit(biopsy) #Omitiing missing values 

#Visualizing data 
#install.packages("reshape2")
library(reshape2)
library(ggplot2)

biop.m = melt(biopsy.v2, id.var="class")
ggplot(data=biop.m, aes(x=class, y=value)) + geom_boxplot() +facet_wrap(~variable,ncol = 3)

library(corrplot)
bc = cor(biopsy.v2[ ,1:9]) #creating an object of the features
corrplot.mixed(bc)
#The correlation coefficients are indicating that we may have a problem with collinearity, in particular, 
#the features of uniform shape and uniform size that are present.

#Partitioning data into training and test dataset
set.seed(123) #random number generator
ind = sample(2, nrow(biopsy.v2), replace=TRUE, prob=c(0.70, 0.3))
train = biopsy.v2[ind==1,] #the training data set
test = biopsy.v2[ind==2,] #the test data set
str(test) #confirm it worked

table(train$class)
prop.table(table(train$class))
table(test$class)
prop.table(table(test$class))

##Modeling and Evaluation 
full.fit = glm(class~., family=binomial, data=train)
summary(full.fit)
confint(full.fit)
#two significant features have confidence intervals that do not cross zero

#Using odds ratio for more data exploration
exp(coef(full.fit))
#all the features except s.size will increase the log odds

#Checking collinearity 
library(car)
vif(full.fit)
#None of the values are greater than the VIF rule of thumb statistic of five, so collinearity does not seem to be a problem

#Model Evaluation 
train$probs = predict(full.fit, type="response")
train$probs[1:5] #inspecting the first 5 predicted probabilities
contrasts(train$class)

train$predict = rep("benign", 474)
train$predict[train$probs>0.5]="malignant"
mean(train$predict==train$class)
#97 percent prediction rate on the training set.

test$prob = predict(full.fit, newdata=test, type="response")
test$predict = rep("benign", 209)
test$predict[test$prob>0.5]="malignant"
table(test$predict, test$class)
mean(test$predict==test$class)
#98 percent prediction rate is quite impressive, However, we must still see if there is room for improvement.

## Logistic regression with Cross validation 
library(bestglm)
train$y=rep(0,474)
train$y[train$class=="malignant"]=1
head(train)

biopsy.cv = train[ ,-10]
head(biopsy.cv)

##Cross Validation 
bestglm(Xy = biopsy.cv, IC="CV", CVArgs=list(Method="HTF", K=10, REP=1), family=binomial)
#We get three features for Best Model such as thick, u.size, and nucl.

#Putting features in glm 
reduce.fit = glm(class~thick+u.size+nucl, family=binomial, data=train)

train$cv.probs = predict(reduce.fit, type="response")
train$cv.predict = rep("benign", 474)
train$cv.predict[train$cv.probs>0.5]="malignant"
table(train$cv.predict, train$class)
#the reduced feature model had two more false negatives than the full model

test$cv.probs = predict(reduce.fit, newdata=test, type="response")
test$predict = rep("benign", 209)
test$predict[test$cv.probs>0.5]="malignant"
table(test$predict, test$class)
#The reduced feature model again produced more false negatives than when all the features were included

#Best subsets with the information criterion set to BIC
bestglm(Xy= biopsy.cv, IC="BIC", family=binomial)

bic.fit=glm(class~thick+adhsn+nucl+n.nuc, family=binomial, data=train)
test$bic.probs = predict(bic.fit, newdata=test, type="response")
test$bic.predict = rep("benign", 209)
test$bic.predict[test$bic.probs>0.5]="malignant"
table(test$bic.predict, test$class)
#Here we have five errors just like the full model.

#Discriminant analysis could be used, as classes are not well-seperated 

