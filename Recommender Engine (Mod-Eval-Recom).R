#install.packages("recommenderlab")
library(recommenderlab)
data(Jester5k)
Jester5k
summary(Jester5k)

#Looking at user10
as(Jester5k[10,], "list")
#mean rating for a user (user 10) and/or the mean rating for a specific joke (joke 1), 
rowMeans(Jester5k[10,])
colMeans(Jester5k[,1])

#histogram, both the raw data and after normalization
hist(getRatings(Jester5k), breaks=100)
hist(getRatings(normalize(Jester5k)), breaks=100)

#Training and test dataset
e = evaluationScheme(Jester5k, method="split", train=0.8, given=15, goodRating=5)
e

#With the train and test data established, we will now begin to model and evaluate the different
#recommenders: user-based, item-based, popular, SVD, PCA, and random


#Modeling, evaluation, and recommendations
recommenderRegistry$get_entries(dataType ="realRatingMatrix")

#putting together the algorithms based on the train data
ubcf = Recommender(getData(e,"train"), "UBCF")
ibcf = Recommender(getData(e,"train"), "IBCF")
svd = Recommender(getData(e, "train"), "SVD")
popular = Recommender(getData(e, "train"), "POPULAR")
#pca = Recommender(getData(e, "train"), "PCA") #Did'nt get implemented
random = Recommender(getData(e, "train"), "RANDOM")

#getting the predicted ratings for the 15 items of the test data for each of the algorithms usimg predict and get data 
user_pred = predict(ubcf, getData(e,"known"),type="ratings")
item_pred = predict(ibcf, getData(e, "known"),type="ratings")
svd_pred = predict(svd, getData(e, "known"),type="ratings")
pop_pred = predict(popular, getData(e, "known"),type="ratings")
#pca_pred = predict(pca, getData(e, "known"),type="ratings")
rand_pred = predict(random, getData(e, "known"), type="ratings")

#examining the error between the predictions and unknown portion of the test data
P1 = calcPredictionAccuracy(user_pred, getData(e,"unknown"))
P1
P2 = calcPredictionAccuracy(item_pred, getData(e,"unknown"))
P3 = calcPredictionAccuracy(svd_pred, getData(e, "unknown"))
P4 = calcPredictionAccuracy(pop_pred, getData(e,"unknown"))
#P5 = calcPredictionAccuracy(pca_pred, getData(e,"unknown"))
P6 = calcPredictionAccuracy(rand_pred, getData(e,"unknown"))
error = rbind(P1,P2,P3,P4,P6)
rownames(error) = c("UBCF", "IBCF", "SVD", "Popular", "Random")
error
#user-based algorithm slightly outperforms SVD and popular also did fairly well

#another way to compare methods using the evaluate() function 
algorithms = list(POPULAR = list(name = "POPULAR"),UBCF =list(name = "UBCF"),IBCF = list(name = "IBCF"))
algorithms

#let's compare the top 5, 10, and 15 joke recommendations
evlist = evaluate(e, algorithms,n=c(5,10,15))

#examining the performance using the avg() 
avg(evlist)
#popular-based and user-based algorithms are almost identical and outperform the item-based one

#ROC comparing TPR and FPR
plot(evlist, legend="topleft", annotate=TRUE)

#precision/recall curve plot 
plot(evlist, "prec", legend="bottomright", annotate=TRUE)
#popular-based and user-based algorithms are almost identical and outperform the item-based one

##Now build a user-based recommendation engine on the full dataset for recommendations
R1 = Recommender(Jester5k, method="UBCF")
R1

#top five recommendations for the first two raters and produce them as a list
recommend = predict(R1, Jester5k[1:2], n=5)
as(recommend, "list")

#rater's specific rating score (raters 300 through 309) and three jokes (71 through 73):
rating = predict(R1, Jester5k[300:309], type="ratings")
rating
as(rating, "matrix")[,71:73]



######building recommendations for those situations where the ratings are binary#######
#turn the ratings into this binary format with 5 or greater as a 1 and less than 5 as 0
Jester.bin = binarize(Jester5k, minRating=5)
#subset of the necessary data
Jester.bin = Jester.bin[rowCounts(Jester.bin)>10]
Jester.bin

set.seed(456)
e.bin = evaluationScheme(Jester.bin, method="cross-validation", k=5, given=10) # Evaluation scheme with 10 items given with 5 runs
algorithms.bin = list("random" = list(name="RANDOM", param=NULL),"popular" = list(name="POPULAR", param=NULL),"UBCF" = list(name="UBCF"))
results.bin = evaluate(e.bin, algorithms.bin, n=c(5,10,15))

plot(results.bin, legend="topleft")
plot(results.bin, "prec", legend="bottomright")
#The user-based algorithm slightly outperforms the popular-based one
