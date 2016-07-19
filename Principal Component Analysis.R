##Data understanding and preparation
library(corrplot) #correlation plot
#install.packages("FactoMineR")
library(FactoMineR) #additional PCA analysis
library(ggplot2) #support scatterplot
#install.packages("GPArotation")
library(GPArotation) #supports rotation
library(psych) #PCA package

url="http://textuploader.com/ae6t4/raw"
nhl = as.data.frame(read.csv(url, header=FALSE))
nhl
str(nhl)
names(nhl)
names(nhl) = c("rank","team","played","wins","losses","OTL","pts","ROW","HROW","RROW","ppc","gg","gag","five","PPP","PKP","shots","sag","sc1","tr1","lead1","lead2","wop","wosp","face")


#Sorting Data 
nhl=nhl[order(nhl$gg),]
nhl[1,2]
nhl[30,2]

#Making data frame based on requirements 
pca.df = nhl[,c(-1:-11)]
pca.df = as.data.frame(lapply(pca.df , as.numeric))
str(pca.df)

##Correlation Plots 
nhl.cor = cor(pca.df)
corrplot(nhl.cor, method="ellipse")

##Component extraction
pca = principal(pca.df, nfactors=5, rotate="none")
pca
#Scree Plot 
plot(pca$values, type="b", ylab="Eigenvalues", xlab="Component")
#Three components look pretty compelling 

##Orthogonal rotation and interpretation
pca.rotate = principal(pca.df, nfactors=3, rotate = "varimax")
pca.rotate

##Creating factor scores from the components
pca.scores = pca.rotate$scores
pca.scores = as.data.frame(pca.scores)
pca.scores


#preparing dataframe forregression analysis 
#converting the total points to numeric and attaching the factor scores as new variables
nhl$pts = as.numeric(nhl$pts)
nhl$Def = pca.scores$RC1
nhl$Off = pca.scores$RC3
nhl$PPlay = pca.scores$RC2

##Regression Analysis 
nhl.lm = lm(pts~Def+Off+PPlay, data=nhl)
summary(nhl.lm)
#overall model is highly significant statistically, with p-value of 1.004e-15 and Adjusted R-squared is over 92 percent.
#power play factor is not statistically significant with p-value of 0.437

#Excluding Power PLay Factor
nhl.lm2 = lm(pts~Def+Off, data=nhl)
#This model still achieves a high Adjusted R-squared value (93.07 percent) with statistically significant factor coefficients

plot(nhl.lm2$fitted.values, nhl$pts, main="Predicted versus Actual",xlab="Predicted",ylab="Actual")
#model did a good job of using two factors to predict the team's success and also highlights the strong linear relationship
#between the principal components and team points

##creating a data frame of the 15 best teams in terms of points and call it nhl.best
nhl$pred = nhl.lm2$fitted.values
nhl=nhl[order(-nhl$pts),]
nhl.best = nhl[1:15,]

p = ggplot(nhl.best, aes(x=pred, y=pts, label=team))
p + geom_point() + geom_text(size=3.5, hjust=.2, vjust=-0.5, angle=15) + xlim(90,120) + ylim(90, 120) + stat_smooth(method="lm", se=FALSE)
#teams below the line underachieved, while those above it, overachieved.

#plotting the teams in relationship to their factor scores.
p2 = ggplot(nhl, aes(x=Def, y=Off, label=team))
p2 + geom_point() + geom_text(size=3, hjust=.2, vjust=-0.5, angle=0) + xlim(-3,3) + ylim(-3,3)
#x axis is the defensive factor scores and the y axis is the offensive factor scores, the NY ISLANDERS with the highest offensive 
#actor scores but one of the worst defensive scores
#Do the most balanced teams such as the NY RANGERS, TAMPA BAY, and others have the best chance to win a title

  


  