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


#logistic regression
st <- Sys.time()
set.seed(3)
n_fold<-1000
folds_i <- sample(rep(1:n_fold, length.out = n))
OUTglm=NULL
TRUTH = NULL
OUTPUT=NULL
for (k in 1:n_fold) {
  testID <- which(folds_i == k) #take 1/k of the data set
  train_set <- morphii_toy[-testID, ]
  test_set <- morphii_toy[testID, ]
  #print(table(train_set$gender))
  
  glmfit=glm(gender ~ . ,data=train_set,family=binomial)
  glmprobs=predict(glmfit,test_set,type="response")
  
  glmpred=rep("F",length(testID))
  glmpred[glmprobs>.5]="M"
  table(glmpred, test_set$gender) ## confusion table
  
  Accuracy=mean(glmpred==test_set$gender) ## Prediction Accuracy
  OUTglm=c(OUTglm, Accuracy)
  
  TRUTH = c(TRUTH, test_set$gender)
  OUTPUT= c(OUTPUT, glmpred)
}
print("lrloocv time taken:")
Sys.time() - st
print("mean:")
mean(OUTglm)
print("se:")
sd(OUTglm)
print("table:")
table(TRUTH,OUTPUT)

#lda
st <- Sys.time()
OUT=NULL
n <- dim(morphii_toy)[1]
for (k in 1:n) {
  testID <- k
  train_set <- morphii_toy[-testID, ]
  test_set <- morphii_toy[testID, ]
  ldafit=lda(gender~., data=train_set)
  ldapred=predict(ldafit, test_set[,1:100])
  ldaclass=ldapred$class
  table(ldaclass,test_set[,101])
  Accuracy=mean(ldaclass==test_set[,101])
  OUT=c(OUT, Accuracy)
}
print("ldaloocv time taken:")
Sys.time() - st
print("mean:")
mean(OUT)
print("se:")
sd(OUT)
print("table:")
table(ldaclass,morphii_toy$gender)

#qda
st <- Sys.time()
OUT=NULL
n <- dim(morphii_toy)[1]
for (k in 1:n) {
  testID <- k
  train_set <- morphii_toy[-testID, ]
  test_set <- morphii_toy[testID, ]
  qdafit=qda(gender~., data=train_set)
  qdapred=predict(qdafit, test_set[,1:100])
  qdaclass=qdapred$class
  table(qdaclass,test_set[,101])
  Accuracy=mean(qdaclass==test_set[,101])
  OUT=c(OUT, Accuracy)
}
print("qdaloocv time taken:")
Sys.time() - st
print("mean:")
mean(OUT)
print("se:")
sd(OUT)
print("table:")
table(qdaclass,morphii_toy$gender)

#knn
st <- Sys.time()
OUTKNN=NULL
for (j in 1:n) {
  set.seed(3)
  test.ID <- which(folds_i == j)
  train_X <- morphii_toy[-test.ID, 1:100]
  train_Y <- morphii_toy[-test.ID, 101]
  test_X <- morphii_toy[test.ID, 1:100]
  test_Y <- morphii_toy[test.ID, 101]
  knnpred=knn(train_X, test_X, train_Y, k=7) 
  table(knnpred,test_Y)
  Accuracy=mean(knnpred==test_Y)
  OUTKNN=c(OUTKNN, Accuracy)
}
print("knnloocv time taken:")
Sys.time() - st
print("mean:")
mean(OUTKNN)
print("se:")
sd(OUTKNN)
print("table:")
table(knnpred,morphii_toy$gender)

#decision tree
starttime <- Sys.time()
OUTtree=NULL
TRUTH = NULL
OUTPUT=NULL
for (k in 1:1000) {
  set.seed(3)
  train_set <- morphii_toy[-k, ]
  test_set <- morphii_toy[k, ]
  
  treefit=tree(gender ~ . ,data=train_set)
  treepred <- predict(treefit,test_set,type="class")
  
  Accuracy=mean(treepred==test_set$gender) ## Prediction Accuracy
  OUTtree=c(OUTtree, Accuracy)
  
  TRUTH = c(TRUTH, test_set$gender)
  OUTPUT= c(OUTPUT, treepred)
}
print("time taken:")
Sys.time() - starttime
print("tree accuracy:")
mean(OUTtree) #acc: 0.789
print("tree SE:")
sd(OUTtree)
print("tree table:")
table(TRUTH,OUTPUT)

#bagging
starttime <- Sys.time()
OUTbag=NULL
TRUTH = NULL
OUTPUT=NULL
dim(morphii_toy)
for (k in 1:1000) {
  set.seed(3)
  train_set <- morphii_toy[-k, ]
  test_set <- morphii_toy[k, ]
  
  bagfit=randomForest(gender ~ . ,data=train_set,mtry=100,ntree=500,importance=TRUE)
  bagpred <- predict(bagfit,test_set)
  
  Accuracy=mean(bagpred==test_set$gender) ## Prediction Accuracy
  OUTbag=c(OUTbag, Accuracy)
  
  TRUTH = c(TRUTH, test_set$gender)
  OUTPUT= c(OUTPUT, bagpred)
}
print("time taken:")
Sys.time() - starttime
print("bagging mean:")
mean(OUTbag) #acc: 0.844
print("bagging SE:")
sd(OUTbag)
print("bagging table:")
table(TRUTH,OUTPUT)

#random forest
starttime <- Sys.time()
OUTrf=NULL
TRUTH = NULL
OUTPUT=NULL
dim(morphii_toy)
for (k in 1:1000) {
  set.seed(3)
  train_set <- morphii_toy[-k, ]
  test_set <- morphii_toy[k, ]
  
  randfit=randomForest(gender ~ . ,data=train_set,mtry=10,ntree=500,importance=TRUE)
  randpred <- predict(randfit,test_set)
  
  Accuracy=mean(randpred==test_set$gender) ## Prediction Accuracy
  OUTrf=c(OUTrf, Accuracy)
  
  TRUTH = c(TRUTH, test_set$gender)
  OUTPUT= c(OUTPUT, randpred)
}
print("time taken:")
Sys.time() - starttime
print("rf mean:")
mean(OUTrf)
print("rf SE:")
sd(OUTrf)
print("rf table:")
table(TRUTH,OUTPUT)

#boosting
starttime <- Sys.time()
OUTboost=NULL
TRUTH = NULL
OUTPUT=NULL
dim(morphii_toy)
for (k in 1:1000) {
  set.seed(3)
  train_set <- morphii_toy[-k, ]
  test_set <- morphii_toy[k, ]
  test_Y=train_set[,101]
  boostfit<- gbm((as.numeric(gender)-1)~.,data=train_set, 
                 distribution = "bernoulli", n.trees = 5000, 
                 shrinkage=0.01, interaction.depth = 2)
  boostpred <- predict(boostfit, test_set, 
                       n.trees = 5000, type = 'response')
  pred=boostpred
  pred=factor(ifelse(pred<=0.5, 0, 1))
  Accuracy <- mean(pred==(as.numeric(test_Y)-1))
  OUTboost=c(OUTboost, Accuracy)
  
  TRUTH = c(TRUTH, test_set$gender)
  OUTPUT= c(OUTPUT, pred)
}
print("time taken:")
Sys.time() - starttime
print("boosting mean:")
mean(OUTboost) #0.791
print("boosting se:")
sd(OUTboost)
print("boosting table:")
table(TRUTH,OUTPUT) 

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