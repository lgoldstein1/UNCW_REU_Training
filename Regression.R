library(gbm)
library(dplyr)
library(class)
library(MASS)
library(tree)
library(randomForest)
library(e1071)
morphii_m <- read.csv("D:/UNCW REU/morphII_cleaned_v2.csv", header=TRUE)
baby <- read.csv("D:/UNCW REU/MorphII_BIF_s7-37_g0.1_max_partial.csv",header=FALSE)
colnames(baby)[colnames(baby)=="V1"] <- "photo"
morphii_m$photo <- substr(morphii_m$photo, 8, 500)
merged <- merge(baby,morphii_m,by="photo",all.x=TRUE)
#fix(merged)
morphii_toy <- merged[,c(2:21,2577)]
#fix(morphii_toy)

lm1 <- lm(age~.,data=morphii_toy)
summary(lm1)
lm1pred <- predict(lm1,morphii_toy)
mean((lm1pred - morphii_toy$age)^2) #112.118

lm2 <- lm(age ~ polym(V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+
                        V15+V16+V17+V18+V19+V20+V21, degree=2), data=morphii_toy)
summary(lm2)
lm2pred <- predict(lm2,morphii_toy)
mean((lm2pred - morphii_toy$age)^2) #118.834

lm3 <- lm(age ~ polym(V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+
                        V15+V16+V17+V18+V19+V20+V21, degree=3), data=morphii_toy)
summary(lm3)
lm3pred <- predict(lm3,morphii_toy)
mean((lm3pred - morphii_toy$age)^2) #118.738

set.seed(3)
knnpred <- knn(morphii_toy[,1:20],morphii_toy[,1:20],morphii_toy[,21],k=2)
mean((as.numeric(knnpred) - morphii_toy$age)^2) #328.527

set.seed(3)
treefit<-tree(age ~ . ,data=morphii_toy)
treepred <- predict(treefit,morphii_toy)
mean((treepred - morphii_toy$age)^2) #106.025

set.seed(3)
bagfit <- randomForest(age ~ . ,
                       data=morphii_toy,mtry=20,ntree=500,importance=TRUE)
bagpred <- predict(bagfit,morphii_toy)
mean((bagpred - morphii_toy$age)^2) #19.103

set.seed(3)
randfit=randomForest(age ~ . ,
                     data=morphii_toy,mtry=7,ntree=500,importance=TRUE)
randpred <- predict(randfit,morphii_toy)
mean((randpred - morphii_toy$age)^2) #20.567

set.seed(3)
boostfit <- gbm(age~.,data=morphii_toy,distribution = "gaussian", n.trees = 5000, 
                shrinkage=0.01, interaction.depth = 2)
boostpred <- predict(boostfit, morphii_toy, 
                     n.trees = 5000)
mean((boostpred - morphii_toy$age)^2) #61.2221

set.seed(3)
svmfit <- svm(age~.,data=morphii_toy,kernel="linear",cost=0.001,scale=FALSE)
svmpred <- predict(svmfit, morphii_toy)
mean((svmpred - morphii_toy$age)^2) #113.445

#barplot: beside = 2, rbind your data together