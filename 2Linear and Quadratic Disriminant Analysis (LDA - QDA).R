#Logistic regression codes need to be run before starting 

##Fisher Discriminant Analysis (FDA)
#Linear Discriminant Analysis (LDA)

#Deleting the columns that we had added to the training and test sets
head(train)
lda.train = train[ ,-11:-13]
lda.train[1:3,]
head(test)
lda.test = test[ ,-11:-14]
lda.test[1:3,]

#Fitting our LDA model
lda.fit = lda(class~., data=lda.train)
lda.fit

plot(lda.fit, type="both")
#there is some overlap in the groups, indicating that there will be some incorrectly classified observations

#Confusion Matrix 
lda.predict = predict(lda.fit)
train$lda = lda.predict$class
table(train$lda, train$class)
#it appears that our LDA model has performed much worse than the logistic regression models

lda.test = predict(lda.fit, newdata = test)
test$lda = lda.test$class
table(test$lda, test$class)
mean(test$lda==test$class)
#not as bad as I thought, given the poor performance on the training data. From a correctly classified perspective, 
#it still did not perform as well as logistic regression (96 percent versus almost 98 percent with logistic regression):

#Quadratic Discriminant Analysis (QDA)
qda.fit = qda(class~., data=lda.train)
qda.fit

#Predictions for train and test data 
qda.predict = predict(qda.fit)
train$qda = qda.predict$class
table(train$qda, train$class)

qda.test = predict(qda.fit, newdata=test)
test$qda = qda.test$class
table(test$qda, test$class)
#QDA classified the test set poorly with 11 incorrect predictions. In particular, it has a high rate of false positives.


