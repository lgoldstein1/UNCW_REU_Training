print("KPCA LOOCV")

library(dplyr)
library(tree)
library(randomForest)
library(gbm)
library(colorspace)
library(MASS)
library(e1071)
library(class)
library(kernlab)
library(glmnet)

#data setup
morphii_m <- read.csv("/pylon5/ac5fphp/lgolds1/morphII_cleaned_v2.csv", 
                      header=TRUE)
baby <- read.csv("/pylon5/ac5fphp/lgolds1/MorphII_BIF_s7-37_g0.1_max_partial.csv",
                 header=FALSE)
colnames(baby)[colnames(baby)=="V1"] <- "photo"
morphii_m$photo <- substr(morphii_m$photo, 8, 500)
merged <- merge(baby,morphii_m,by="photo",all.x=TRUE)
bif <- 2568
vars <- c(2574,2575,2577)
morphii_toy <- merged[,c(1:bif+1,vars)]
n=dim(morphii_toy)[1]
m=dim(morphii_toy)[2]
set.seed(3)
n <- 1000
n_fold<-1000
folds_i <- sample(rep(1:n_fold, length.out = n))

feature <- morphii_toy[,1:2567]
cormat1<-cor(feature) 
#cumsum(lam)/sum(lam)
loadmat<-eigen(cormat1)$vectors[,1:108]
transfeature<-as.matrix(feature)%*%loadmat


p <- 2567
n <- 1000
y <- 2570
m2feature <- morphii_toy[,1:2567]
nfold<-1000
gend <- morphii_toy$gender

fold.id.group<-function(fseed=3, fid, numcv)
{
  set.seed(fseed);
  fn<-length(fid);
  shaf<-sample(fid);
  sub.size<-ceiling(fn/numcv);
  return(lapply(1:numcv, function(x) shaf[((x-1)*sub.size+1): min(fn,x*sub.size)]));
  # the result is a list with each row contains ids belongs to the same testing group
}
groupinfo<-fold.id.group(fid=c(1:n),numcv=nfold)
x <- 1000


#logreg
print("log regression")
st <- Sys.time()
OUTglm<-NULL
TRUTH<-NULL
OUTPUT<-NULL
cvfun<-function(x, feature=m2feature)
{
  rawtrain <- morphii_toy[-groupinfo[[x]],1:2567]
  trainy <- as.numeric(morphii_toy[-groupinfo[[x]],2570]) - 1
  rawtest <- morphii_toy[groupinfo[[x]],1:2567]
  testy <- as.numeric(morphii_toy[groupinfo[[x]],2570]) -1
  trainprcomp <- kpca(~.,data=as.data.frame(rawtrain),kernel="rbfdot",
                      kpar=list(sigma=1e-10),features=108)
  trainfeature <- trainprcomp@rotated
  testfeature <- predict(trainprcomp,rawtest)
  train_set <- as.data.frame(cbind(trainy,trainfeature))
  test_set <- as.data.frame(cbind(testy,testfeature))
  names(train_set)[1] <- "gender"
  names(test_set)[1] <- "gender"
  
  glmfit=glm(gender ~ . ,data=train_set,family=binomial,maxit=70)
  glmprobs=predict(glmfit,test_set,type="response")
  
  glmpred=rep(0,200)
  glmpred[glmprobs>.5]=1
  
  
  Accuracy=mean(glmpred==test_set$gender) ## Prediction Accuracy
  assign("OUTglm",c(OUTglm,Accuracy),envir=.GlobalEnv)
  assign("TRUTH",c(TRUTH,test_set$gender),envir=.GlobalEnv)
  assign("OUTPUT",c(OUTPUT,glmpred),envir=.GlobalEnv)
  return(Accuracy)
}
result<-lapply(c(1:nfold),cvfun)
print(Sys.time() - st)
result<- unlist(result)
print("results")
mean(result)
sd(result)
if(length(TRUTH)==length(OUTPUT)) {table(TRUTH,OUTPUT)}

#lda
st <- Sys.time()
print("LDA")
OUTlda<-NULL
TRUTH<-NULL
OUTPUT<-NULL
cvfun<-function(x, feature=m2feature)
{
  rawtrain <- morphii_toy[-groupinfo[[x]],1:2567]
  trainy <- as.numeric(morphii_toy[-groupinfo[[x]],2570]) - 1
  rawtest <- morphii_toy[groupinfo[[x]],1:2567]
  testy <- as.numeric(morphii_toy[groupinfo[[x]],2570]) -1
  trainprcomp <- kpca(~.,data=as.data.frame(rawtrain),kernel="rbfdot",
                      kpar=list(sigma=1e-10),features=108)
  trainfeature <- trainprcomp@rotated
  testfeature <- predict(trainprcomp,rawtest)
  train_set <- as.data.frame(cbind(trainy,trainfeature))
  test_set <- as.data.frame(cbind(testy,testfeature))
  names(train_set)[1] <- "gender"
  names(test_set)[1] <- "gender"
  
  ldafit=lda(gender~., data=train_set)
  ldapred=predict(ldafit, test_set,type="response")
  ldaclass=ldapred$class
  Accuracy=mean(ldaclass==test_set[,1])
  
  assign("OUTlda",c(OUTlda,Accuracy),envir=.GlobalEnv)
  OUTlda=c(OUTlda, Accuracy)
  
  assign("TRUTH",c(TRUTH,test_set$gender),envir=.GlobalEnv)
  assign("OUTPUT",c(OUTPUT,ldaclass),envir=.GlobalEnv)
  return(Accuracy)
}
result<-lapply(c(1:nfold),cvfun)
print(Sys.time() - st)
result<- unlist(result)
mean(result)
sd(result)
if(length(TRUTH)==length(OUTPUT)) {table(TRUTH,OUTPUT)}

#qda
print("QDA")
st <- Sys.time()
OUTqda<-NULL
TRUTH<-NULL
OUTPUT<-NULL
cvfun<-function(x, feature=m2feature)
{
  rawtrain <- morphii_toy[-groupinfo[[x]],1:2567]
  trainy <- as.numeric(morphii_toy[-groupinfo[[x]],2570]) - 1
  rawtest <- morphii_toy[groupinfo[[x]],1:2567]
  testy <- as.numeric(morphii_toy[groupinfo[[x]],2570]) -1
  trainprcomp <- kpca(~.,data=as.data.frame(rawtrain),kernel="rbfdot",
                      kpar=list(sigma=1e-10),features=108)
  trainfeature <- trainprcomp@rotated
  testfeature <- predict(trainprcomp,rawtest)
  train_set <- as.data.frame(cbind(trainy,trainfeature))
  test_set <- as.data.frame(cbind(testy,testfeature))
  names(train_set)[1] <- "gender"
  names(test_set)[1] <- "gender"
  
  qdafit=qda(gender~., data=train_set)
  qdapred=predict(qdafit, test_set)
  qdaclass=qdapred$class
  Accuracy=mean(qdaclass==test_set[,1])
  
  assign("OUTqda",c(OUTqda,Accuracy),envir=.GlobalEnv)
  assign("TRUTH",c(TRUTH,test_set$gender),envir=.GlobalEnv)
  assign("OUTPUT",c(OUTPUT,qdaclass),envir=.GlobalEnv)
  return(Accuracy)
}
result<-lapply(c(1:nfold),cvfun)
print(Sys.time() - st)
result<- unlist(result)
mean(result)
sd(result)
if(length(TRUTH)==length(OUTPUT)) {table(TRUTH,OUTPUT)}

#knn
print("KNN")
st <- Sys.time()
OUTknn<-NULL
TRUTH<-NULL
OUTPUT<-NULL
cvfun<-function(x, feature=m2feature)
{
  rawtrain <- morphii_toy[-groupinfo[[x]],1:2567]
  trainy <- as.numeric(morphii_toy[-groupinfo[[x]],2570]) - 1
  rawtest <- morphii_toy[groupinfo[[x]],1:2567]
  testy <- as.numeric(morphii_toy[groupinfo[[x]],2570]) -1
  trainprcomp <- kpca(~.,data=as.data.frame(rawtrain),kernel="rbfdot",
                      kpar=list(sigma=1e-10),features=108)
  trainfeature <- trainprcomp@rotated
  testfeature <- predict(trainprcomp,rawtest)
  train_set <- as.data.frame(cbind(trainy,trainfeature))
  test_set <- as.data.frame(cbind(testy,testfeature))
  names(train_set)[1] <- "gender"
  names(test_set)[1] <- "gender"
  
  knnpred=knn(trainfeature, testfeature, trainy, k=3) 
  table(knnpred,testy)
  Accuracy=mean(knnpred==testy)
  
  assign("OUTknn",c(OUTknn,Accuracy),envir=.GlobalEnv)
  assign("TRUTH",c(TRUTH,test_set$gender),envir=.GlobalEnv)
  assign("OUTPUT",c(OUTPUT,knnpred),envir=.GlobalEnv)
  return(Accuracy)
}
result<-lapply(c(1:nfold),cvfun)
print(Sys.time() - st)
result<- unlist(result)
mean(result)
sd(result)
if(length(TRUTH)==length(OUTPUT)) {table(TRUTH,OUTPUT)}

#tree
st <- Sys.time()
print("tree")
OUTtree<-NULL
TRUTH<-NULL
OUTPUT<-NULL
cvfun<-function(x, feature=m2feature)
{
  rawtrain <- morphii_toy[-groupinfo[[x]],1:2567]
  trainy <- as.numeric(morphii_toy[-groupinfo[[x]],2570]) - 1
  rawtest <- morphii_toy[groupinfo[[x]],1:2567]
  testy <- as.numeric(morphii_toy[groupinfo[[x]],2570]) -1
  trainprcomp <- kpca(~.,data=as.data.frame(rawtrain),kernel="rbfdot",
                      kpar=list(sigma=1e-10),features=108)
  trainfeature <- trainprcomp@rotated
  testfeature <- predict(trainprcomp,rawtest)
  train_set <- as.data.frame(cbind(trainy,trainfeature))
  test_set <- as.data.frame(cbind(testy,testfeature))
  names(train_set)[1] <- "gender"
  names(test_set)[1] <- "gender"
  train_set$gender <- as.factor(train_set$gender)
  test_set$gender <- as.factor(test_set$gender)
  
  treefit=tree(gender ~ . ,data=train_set)
  treepred <- predict(treefit,test_set,type="class")
  Accuracy=mean(treepred==test_set$gender)
  
  assign("OUTtree",c(OUTtree,Accuracy),envir=.GlobalEnv)
  assign("TRUTH",c(TRUTH,test_set$gender),envir=.GlobalEnv)
  assign("OUTPUT",c(OUTPUT,treepred),envir=.GlobalEnv)
  return(Accuracy)
}
result<-lapply(c(1:nfold),cvfun)
print(Sys.time() - st)
result<- unlist(result)
mean(result)
sd(result)
if(length(TRUTH)==length(OUTPUT)) {table(TRUTH,OUTPUT)}

#bagging
print("bagging")
st <- Sys.time()
OUTbag<-NULL
TRUTH<-NULL
OUTPUT<-NULL
cvfun<-function(x, feature=m2feature)
{
  rawtrain <- morphii_toy[-groupinfo[[x]],1:2567]
  trainy <- as.numeric(morphii_toy[-groupinfo[[x]],2570]) - 1
  rawtest <- morphii_toy[groupinfo[[x]],1:2567]
  testy <- as.numeric(morphii_toy[groupinfo[[x]],2570]) -1
  trainprcomp <- kpca(~.,data=as.data.frame(rawtrain),kernel="rbfdot",
                      kpar=list(sigma=1e-10),features=108)
  trainfeature <- trainprcomp@rotated
  testfeature <- predict(trainprcomp,rawtest)
  train_set <- as.data.frame(cbind(trainy,trainfeature))
  test_set <- as.data.frame(cbind(testy,testfeature))
  names(train_set)[1] <- "gender"
  names(test_set)[1] <- "gender"
  train_set$gender <- as.factor(train_set$gender)
  test_set$gender <- as.factor(test_set$gender)
  
  bagfit=randomForest(gender ~ . ,data=train_set,
                      mtry=100,ntree=500,importance=TRUE)
  bagpred <- predict(bagfit,test_set)
  Accuracy=mean(bagpred==test_set$gender)
  
  assign("OUTbag",c(OUTbag,Accuracy),envir=.GlobalEnv)
  assign("TRUTH",c(TRUTH,test_set$gender),envir=.GlobalEnv)
  assign("OUTPUT",c(OUTPUT,bagpred),envir=.GlobalEnv)
  return(Accuracy)
}
result<-lapply(c(1:nfold),cvfun)
print(Sys.time() - st)
result<- unlist(result)
mean(result)
sd(result)
if(length(TRUTH)==length(OUTPUT)) {table(TRUTH,OUTPUT)}


#random forest
print("random forest")
st <- Sys.time()
OUTrf<-NULL
TRUTH<-NULL
OUTPUT<-NULL
cvfun<-function(x, feature=m2feature)
{
  rawtrain <- morphii_toy[-groupinfo[[x]],1:2567]
  trainy <- as.numeric(morphii_toy[-groupinfo[[x]],2570]) - 1
  rawtest <- morphii_toy[groupinfo[[x]],1:2567]
  testy <- as.numeric(morphii_toy[groupinfo[[x]],2570]) -1
  trainprcomp <- kpca(~.,data=as.data.frame(rawtrain),kernel="rbfdot",
                      kpar=list(sigma=1e-10),features=108)
  trainfeature <- trainprcomp@rotated
  testfeature <- predict(trainprcomp,rawtest)
  train_set <- as.data.frame(cbind(trainy,trainfeature))
  test_set <- as.data.frame(cbind(testy,testfeature))
  names(train_set)[1] <- "gender"
  names(test_set)[1] <- "gender"
  train_set$gender <- as.factor(train_set$gender)
  test_set$gender <- as.factor(test_set$gender)
  
  randfit=randomForest(gender ~ . ,data=train_set,mtry=10,
                       ntree=500,importance=TRUE)
  randpred <- predict(randfit,test_set)
  Accuracy=mean(randpred==test_set$gender)
  
  assign("OUTrf",c(OUTrf,Accuracy),envir=.GlobalEnv)
  assign("TRUTH",c(TRUTH,test_set$gender),envir=.GlobalEnv)
  assign("OUTPUT",c(OUTPUT,randpred),envir=.GlobalEnv)
  return(Accuracy)
}
result<-lapply(c(1:nfold),cvfun)
print(Sys.time() - st)
result<- unlist(result)
mean(result)
sd(result)
if(length(TRUTH)==length(OUTPUT)) {table(TRUTH,OUTPUT)}

#boosting
print("boosting")
st <- Sys.time()
OUTboost<-NULL
TRUTH<-NULL
OUTPUT<-NULL
cvfun<-function(x, feature=m2feature)
{
  rawtrain <- morphii_toy[-groupinfo[[x]],1:2567]
  trainy <- as.numeric(morphii_toy[-groupinfo[[x]],2570]) - 1
  rawtest <- morphii_toy[groupinfo[[x]],1:2567]
  testy <- as.numeric(morphii_toy[groupinfo[[x]],2570]) -1
  trainprcomp <- kpca(~.,data=as.data.frame(rawtrain),kernel="rbfdot",
                      kpar=list(sigma=1e-10),features=108)
  trainfeature <- trainprcomp@rotated
  testfeature <- predict(trainprcomp,rawtest)
  train_set <- as.data.frame(cbind(trainy,trainfeature))
  test_set <- as.data.frame(cbind(testy,testfeature))
  names(train_set)[1] <- "gender"
  names(test_set)[1] <- "gender"
  train_set$gender <- as.factor(train_set$gender)
  test_set$gender <- as.factor(test_set$gender)
  
  boostfit<- gbm((as.numeric(gender)-1)~.,data=train_set, 
                 distribution = "bernoulli", n.trees = 5000, 
                 shrinkage=0.01, interaction.depth = 4)
  boostpred <- predict(boostfit, test_set, 
                       n.trees = 5000, type = 'response')
  pred=boostpred
  pred=factor(ifelse(pred<=0.5, 0, 1))
  Accuracy <- mean(pred==(as.numeric(testy)-1))
  
  assign("OUTboost",c(OUTboost,Accuracy),envir=.GlobalEnv)
  assign("TRUTH",c(TRUTH,test_set$gender),envir=.GlobalEnv)
  assign("OUTPUT",c(OUTPUT,pred),envir=.GlobalEnv)
  return(Accuracy)
}
result<-lapply(c(1:nfold),cvfun)
print(Sys.time() - st)
result<- unlist(result)
mean(result)
sd(result)
if(length(TRUTH)==length(OUTPUT)) {table(TRUTH,OUTPUT)}

#svm
print("svm")
st <- Sys.time()
OUTsvm<-NULL
TRUTH<-NULL
OUTPUT<-NULL
cvfun<-function(x, feature=m2feature)
{
  rawtrain <- morphii_toy[-groupinfo[[x]],1:2567]
  trainy <- as.numeric(morphii_toy[-groupinfo[[x]],2570]) - 1
  rawtest <- morphii_toy[groupinfo[[x]],1:2567]
  testy <- as.numeric(morphii_toy[groupinfo[[x]],2570]) -1
  trainprcomp <- kpca(~.,data=as.data.frame(rawtrain),kernel="rbfdot",
                      kpar=list(sigma=1e-10),features=108)
  trainfeature <- trainprcomp@rotated
  testfeature <- predict(trainprcomp,rawtest)
  train_set <- as.data.frame(cbind(trainy,trainfeature))
  test_set <- as.data.frame(cbind(testy,testfeature))
  names(train_set)[1] <- "gender"
  names(test_set)[1] <- "gender"
  train_set$gender <- as.factor(train_set$gender)
  test_set$gender <- as.factor(test_set$gender)
  
  svmfit <- svm(gender~.,data=train_set,kernel="linear",cost=0.001,
                scale=FALSE)
  svmpred <- predict(svmfit, test_set)
  Accuracy <- mean(svmpred==test_set$gender)
  
  assign("OUTsvm",c(OUTsvm,Accuracy),envir=.GlobalEnv)
  assign("TRUTH",c(TRUTH,test_set$gender),envir=.GlobalEnv)
  assign("OUTPUT",c(OUTPUT,svmpred),envir=.GlobalEnv)
  return(Accuracy)
}
result<-lapply(c(1:nfold),cvfun)
print(Sys.time() - st)
result<- unlist(result)
mean(result)
sd(result)
if(length(TRUTH)==length(OUTPUT)) {table(TRUTH,OUTPUT)}