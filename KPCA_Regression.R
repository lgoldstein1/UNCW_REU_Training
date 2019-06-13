library(dplyr)
library(tree)
library(randomForest)
library(gbm)
library(colorspace)
library(MASS)
library(e1071)
library(class)
library(kernlab)

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

#kpca
rawtrain <- morphii_toy[1:800,1:2567]
trainy <- morphii_toy[1:800,]$age
rawtest <- morphii_toy[801:1000,1:2567]
testy <- morphii_toy[801:1000,]$age
trainprcomp <- kpca(~.,data=as.data.frame(rawtrain),kernel="rbfdot",
                    kpar=list(sigma=1e-10),features=108)
trainfeature <- trainprcomp@rotated
testfeature <- predict(trainprcomp,rawtest)
train_set <- as.data.frame(cbind(trainy,trainfeature))
test_set <- as.data.frame(cbind(testy,testfeature))
names(train_set)[1] <- "age"
names(test_set)[1] <- "age"


lm1 <- lm(age~.,data=train_set)
summary(lm1)
lm1pred <- predict(lm1,test_set)
mean((lm1pred - test_set$age)^2) #126.24

set.seed(3)
knnpred <- knn(train_set[,2:109],test_set[,2:109],train_set[,1],k=3)
mean((as.numeric(knnpred) - test_set$age)^2) #462.06

set.seed(3)
treefit<-tree(age ~ . ,data=train_set)
treepred <- predict(treefit,test_set)
mean((treepred - test_set$age)^2) #155.190

set.seed(3)
bagfit <- randomForest(age ~ . ,
                       data=train_set,mtry=20,ntree=500,importance=TRUE)
bagpred <- predict(bagfit,test_set)
mean((bagpred - test_set$age)^2) #114.544

set.seed(3)
randfit=randomForest(age ~ . ,
                     data=train_set,mtry=7,ntree=500,importance=TRUE)
randpred <- predict(randfit,test_set)
mean((randpred - test_set$age)^2) #111.943

set.seed(3)
boostfit <- gbm(age~.,data=train_set,distribution = "gaussian", n.trees = 5000, 
                shrinkage=0.01, interaction.depth = 4)
boostpred <- predict(boostfit, test_set, 
                     n.trees = 5000)
mean((boostpred - test_set$age)^2) #130.581

set.seed(3)
svmfit <- svm(age~.,data=train_set,kernel="linear",cost=0.001,scale=FALSE)
svmpred <- predict(svmfit, test_set)
mean((svmpred - test_set$age)^2) #128.505

#kpca plots
mses <- c(126.24,462.06,155.190,114.544,111.943,130.581,128.505)
namesss <- c("Linear","KNN","Tree","Bagging","RF","Boost","SVM")
barplot(mses,names=namesss,col=sequential_hcl(7,"sunset"),
        main="MSEs of Age Regression Using KPCA",
        xlab="Method",ylab="MSE")
