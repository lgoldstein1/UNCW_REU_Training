library(dplyr)
library(tree)
library(randomForest)
library(gbm)
library(colorspace)
library(MASS)
library(e1071)
library(class)

#data setup
morphii_m <- read.csv("D:/UNCW REU/morphII_cleaned_v2.csv", header=TRUE)
baby <- read.csv("D:/UNCW REU/MorphII_BIF_s7-37_g0.1_max_partial.csv",header=FALSE)
colnames(baby)[colnames(baby)=="V1"] <- "photo"
morphii_m$photo <- substr(morphii_m$photo, 8, 500)
merged <- merge(baby,morphii_m,by="photo",all.x=TRUE)
bif <- 2568
vars <- c(2574,2575,2577)
morphii_toy <- merged[,c(2:bif+1,vars)]
n=dim(morphii_toy)[1]
m=dim(morphii_toy)[2]
set.seed(3)
set.seed(3)
n <- 1000
n_fold<-5
folds_i <- sample(rep(1:n_fold, length.out = n))

#####princomp
rawtrain <- morphii_toy[1:800,1:2567]
trainy <- morphii_toy[1:800,]$age
rawtest <- morphii_toy[801:1000,1:2567]
testy <- morphii_toy[801:1000,]$age
trainprcomp <- prcomp(rawtrain,retx=TRUE,center=TRUE,scale=TRUE)
rot <- trainprcomp$rotation
rot <- rot[,1:108]
transfeature<-as.matrix(rawtrain)%*%rot
testfeature<-as.matrix(rawtest)%*%rot
train_set <- as.data.frame(cbind(trainy,transfeature))
test_set <- as.data.frame(cbind(testy,testfeature))
names(train_set)[1] <- "age"
names(test_set)[1] <- "age"

lm1 <- lm(age~.,data=train_set)
summary(lm1)
lm1pred <- predict(lm1,test_set)
mean((lm1pred - test_set$age)^2) #86.216

set.seed(3)
knnpred <- knn(train_set[,2:109],test_set[,2:109],train_set[,1],k=3)
mean((as.numeric(knnpred) - test_set$age)^2) #207.955

set.seed(3)
treefit<-tree(age ~ . ,data=train_set)
treepred <- predict(treefit,test_set)
mean((treepred - test_set$age)^2) #190.825

set.seed(3)
bagfit <- randomForest(age ~ . ,
                       data=train_set,mtry=20,ntree=500,importance=TRUE)
bagpred <- predict(bagfit,test_set)
mean((bagpred - test_set$age)^2) #161.050

set.seed(3)
randfit=randomForest(age ~ . ,
                     data=train_set,mtry=7,ntree=500,importance=TRUE)
randpred <- predict(randfit,test_set)
mean((randpred - test_set$age)^2) #174.008

set.seed(3)
boostfit <- gbm(age~.,data=train_set,distribution = "gaussian", n.trees = 5000, 
                shrinkage=0.01, interaction.depth = 4)
boostpred <- predict(boostfit, test_set, 
                     n.trees = 5000)
mean((boostpred - test_set$age)^2) #101.604

set.seed(3)
svmfit <- svm(age~.,data=train_set,kernel="linear",cost=0.001,scale=FALSE)
svmpred <- predict(svmfit, test_set)
mean((svmpred - test_set$age)^2) #86.339

#pca plots
pcamses <- c(86.216,207.955,190.825,161.050,174.008,101.604,86.339)
namesss <- c("Linear","KNN","Tree","Bagging","RF","Boost","SVM")
barplot(pcamses,names=namesss,col=sequential_hcl(7,"sunset"),
        main="MSEs of Age Regression Using PCA",
        xlab="Method",ylab="MSE")
