library(dplyr)
library(tree)
library(randomForest)
library(gbm)
library(e1071)
library(MASS)

#data setup

morphii_m <- read.csv("/pylon5/ac5fphp/lgolds1/morphII_cleaned_v2.csv", 
                      header=TRUE)
baby <- read.csv("/pylon5/ac5fphp/lgolds1/MorphII_BIF_s7-37_g0.1_max_partial.csv",
                 header=FALSE)
colnames(baby)[colnames(baby)=="V1"] <- "photo"
morphii_m$photo <- substr(morphii_m$photo, 8, 500)
merged <- merge(baby,morphii_m,by="photo",all.x=TRUE)
bif <- 100
morphii_toy <- merged[,c(2:(bif+1),2575)]
n=dim(morphii_toy)[1]
m=dim(morphii_toy)[2]
print(c(n, m))
set.seed(3)
data <- merged[,c(2:(bif+1),2575)]
len <- dim(merged)[1]
set.seed(3)
n_fold<-1000
folds_i <- sample(rep(1:n_fold, length.out = n))

#SVM
st <- Sys.time()
OUTsvm=NULL
TRUTH = NULL
OUTPUT=NULL
for (k in 1:n_fold) {
  set.seed(3)
  testID <- which(folds_i == k) #take 1/k of the data set
  train_set <- morphii_toy[-testID, ]
  test_set <- morphii_toy[testID, ]
  svmfit <- svm(gender~.,data=train_set,kernel="linear",cost=0.001,
                scale=FALSE)
  svmpred <- predict(svmfit, test_set)
  Accuracy <- mean(svmpred==test_set$gender)
  OUTsvm=c(OUTsvm, Accuracy)
  
  TRUTH = c(TRUTH, test_set$gender)
  OUTPUT= c(OUTPUT, svmpred)
}
print("Time taken:")
Sys.time() - st
print("Mean:")
mean(OUTsvm) #0.855
print("SD:")
sd(OUTsvm) #.015
print("Table:")
table(TRUTH,OUTPUT) 