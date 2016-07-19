#Run Logistic Regression codes before starting 

library(ROCR)
#building poor performing model and calling it bad.fit 
bad.fit = glm(class~thick, family=binomial, data=test)
test$bad.probs = predict(bad.fit, type="response") #save probabilities

#performance object with the TPR and FPR
pred.full = prediction(test$prob, test$class)
perf.full = performance(pred.full, "tpr", "fpr")
plot(perf.full, main="ROC", col=1)

#Building model using BIC ((refer to the Logistic regression with cross-validation section)
pred.bic = prediction(test$bic.probs, test$class)
perf.bic = performance(pred.bic, "tpr", "fpr")
plot(perf.bic, col=2, add=TRUE)

pred.bad = prediction(test$bad, test$class)
perf.bad = performance(pred.bad, "tpr", "fpr")
plot(perf.bad, col=3, add=TRUE)
legend(0.6, 0.6, c("FULL", "BIC", "BAD"),1:3)

#Computing AUC 
auc.full = performance(pred.full, "auc")
auc.full
#we are looking for are under the Slot "y.values"

#output for the other two models of interest,
auc.bic = performance(pred.bic, "auc")
auc.bic

auc.bad = performance(pred.bad, "auc")
auc.bad

#The AUCs were 99.7 percent for the full model, 99.4 percent for the BIC model, and 89.6 percent for the bad model. 
#the full model and the BIC model have no difference in predictive powers between them

#A simple solution would be to rerandomize the train and test sets and try this analysis again, perhaps 
#using a 60/40 split and different randomization seed




