library(dplyr)
library(tree)
library(randomForest)
library(gbm)
library(colorspace)
library(MASS)
library(e1071)

#data setup
morphii_m <- read.csv("D:/UNCW REU/morphII_cleaned_v2.csv", header=TRUE)
baby <- read.csv("D:/UNCW REU/MorphII_BIF_s7-37_g0.1_max_partial.csv",header=FALSE)
colnames(baby)[colnames(baby)=="V1"] <- "photo"
morphii_m$photo <- substr(morphii_m$photo, 8, 500)
merged <- merge(baby,morphii_m,by="photo",all.x=TRUE)
bif <- 100
fix(morphii_m)
morphii_toy <- merged[,c(2:(bif+1),2575)]
#write.csv(morphii_toy,file="morphii_toy.csv")
n=dim(morphii_toy)[1]
m=dim(morphii_toy)[2]
print(c(n, m))
set.seed(3)
#data <- merged[,c(2:(bif+1),2575)]
len <- dim(merged)[1]
set.seed(3)
n_fold<-5
folds_i <- sample(rep(1:n_fold, length.out = n))


###k-fold cv

#logistic regression

set.seed(3)
n_fold<-5
folds_i <- sample(rep(1:n_fold, length.out = n))
OUTglm=NULL
TRUTH = NULL
OUTPUT=NULL
for (k in 1:n_fold) {
  testID <- which(folds_i == k) #take 1/k of the data set
  train_set <- morphii_toy[-testID, ]
  test_set <- morphii_toy[testID, ]
  print(table(train_set$gender))
  
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
print(OUTglm)
lrcvacc <- mean(OUTglm)
sd(OUTglm) #se: 0.020
boxplot(OUTglm,col="orange")
ConfusionTable=table(TRUTH,OUTPUT)
print(ConfusionTable)
Overallaccuracy=sum(diag(ConfusionTable))/sum(ConfusionTable) 
print(Overallaccuracy) #accuracy: 0.843
table(TRUTH,OUTPUT)
778/(770+89) #sensitivity: 0.906
68/(68+65) #specificity: 0.511


#lda
OUTLDA=NULL
TRUTH = NULL
OUTPUT=NULL
for (k in 1:n_fold) {
  testID <- which(folds_i == k)
  train_set <- morphii_toy[-testID, ]
  test_set <- morphii_toy[testID, ]
  ldafit=lda(gender~., data=train_set)
  ldapred=predict(ldafit, test_set[,1:100])
  ldaclass=ldapred$class
  table(ldaclass,test_set[,101])
  Accuracy=mean(ldaclass==test_set[,101])
  OUTLDA=c(OUTLDA, Accuracy)
  
  TRUTH = c(TRUTH, test_set[,101])
  OUTPUT= c(OUTPUT, ldaclass)
}
print(OUTLDA)
ldacvacc <- mean(OUTLDA) #overall accuracy: 0.853
sd(OUTLDA) #se: 0.028
table(ldaclass,test_set[,101])
161/(161+9) #sensitivity: 0.947
16/(16+14) #specificity: 0.533

#qda
OUT_QDA<-NULL
TRUTH <- NULL
OUTPUT<-NULL
for (k in 1:n_fold) {
  testID <- which(folds_i == k)
  train_set <- morphii_toy[-testID, ]
  print(table(train_set$gender))
  test_set <- morphii_toy[testID, ]
  qdafit=qda(gender~., data=train_set)
  qdapred=predict(qdafit, test_set[,1:100])
  qdaclass=qdapred$class
  table(qdaclass,test_set[,101])
  Accuracy=mean(qdaclass==test_set[,101])
  OUT_QDA=c(OUT_QDA, Accuracy)
  
  TRUTH = c(TRUTH, test_set[,101])
  OUTPUT= c(OUTPUT, qdaclass)
}
print(OUT_QDA)
mean(OUT_QDA)
sd(OUT_QDA) #se: 0.015
boxplot(OUT_QDA,col="orange")
qdacvacc <- mean(TRUTH==OUTPUT) #accuracy: 0.843
#sensitivity: 1.00
#specificity: 0.00

knn
OUTKNN=NULL
for (q in 2:7) {
  OUTKNN=NULL
  for (j in 1:n_fold) {
    set.seed(3)
    test.ID <- which(folds_i == j)
    train_X <- morphii_toy[-test.ID, 1:100]
    train_Y <- morphii_toy[-test.ID, 101]
    test_X <- morphii_toy[test.ID, 1:100]
    test_Y <- morphii_toy[test.ID, 101]
    knnpred=knn(train_X, test_X, train_Y, k=q) 
    table(knnpred,test_Y)
    Accuracy=mean(knnpred==test_Y)
    OUTKNN=c(OUTKNN, round(Accuracy,2))
  }
  print(c(round(q,0),OUTKNN))
  print("overall mean:")
  print(round(mean(OUTKNN),5)) ##overall accuracy
  round(sd(OUTKNN),2)
} #optimal k is 7
boxplot(OUT.KNN,col="orange")

OUTKNN=NULL
for (j in 1:n_fold) {
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
OUTKNN
knncvacc <- mean(OUTKNN) #accuracy: 0.85
sd(OUTKNN) #se: 0.019
table(knnpred,test_Y)
167/(167+3) #0.982
8/(22+8) #0.267

#decision tree
OUTtree=NULL
TRUTH = NULL
OUTPUT=NULL
for (k in 1:n_fold) {
  set.seed(3)
  testID <- which(folds_i == k) #take 1/k of the data set
  train_set <- morphii_toy[-testID, ]
  test_set <- morphii_toy[testID, ]
  
  treefit=tree(gender ~ . ,data=train_set)
  treepred <- predict(treefit,test_set,type="class")
  
  Accuracy=mean(treepred==test_set$gender) ## Prediction Accuracy
  OUTtree=c(OUTtree, Accuracy)
  
  TRUTH = c(TRUTH, test_set$gender)
  OUTPUT= c(OUTPUT, treepred)
}
OUTtree
mean(OUTtree) #acc: 0.793
sd(OUTtree) #.024
table(TRUTH,OUTPUT)
731/(731+95) #sens: 0.883
62/(62+112) #spec: 0.393

#bagging
OUTbag=NULL
TRUTH = NULL
OUTPUT=NULL
dim(morphii_toy)
for (k in 1:n_fold) {
  set.seed(3)
  testID <- which(folds_i == k) #take 1/k of the data set
  train_set <- morphii_toy[-testID, ]
  test_set <- morphii_toy[testID, ]
  
  bagfit=randomForest(gender ~ . ,data=train_set,mtry=100,ntree=500,importance=TRUE)
  bagpred <- predict(bagfit,test_set)
  
  Accuracy=mean(bagpred==test_set$gender) ## Prediction Accuracy
  OUTbag=c(OUTbag, Accuracy)
  
  TRUTH = c(TRUTH, test_set$gender)
  OUTPUT= c(OUTPUT, bagpred)
}
OUTbag
mean(OUTbag) #acc: 0.837
sd(OUTbag) #.019
table(TRUTH,OUTPUT)
807/(807+127) #sens: 0.864
30/(30+36) #spec: 0.455

#random forest
OUTrf=NULL
TRUTH = NULL
OUTPUT=NULL
dim(morphii_toy)
for (k in 1:n_fold) {
  set.seed(3)
  testID <- which(folds_i == k) #take 1/k of the data set
  train_set <- morphii_toy[-testID, ]
  test_set <- morphii_toy[testID, ]
  
  randfit=randomForest(gender ~ . ,data=train_set,mtry=10,ntree=500,importance=TRUE)
  randpred <- predict(randfit,test_set)
  
  Accuracy=mean(randpred==test_set$gender) ## Prediction Accuracy
  OUTrf=c(OUTrf, Accuracy)
  
  TRUTH = c(TRUTH, test_set$gender)
  OUTPUT= c(OUTPUT, randpred)
}
OUTrf
mean(OUTrf) #acc: 0.85
sd(OUTrf) #.023
table(TRUTH,OUTPUT)
828/(828+135) #sens: 0.860
22/(22+15) #spec: 0.595

#boosting

OUTboost=NULL
TRUTH = NULL
OUTPUT=NULL
dim(morphii_toy)
for (k in 1:n_fold) {
  set.seed(3)
  testID <- which(folds_i == k) #take 1/k of the data set
  train_set <- morphii_toy[-testID, ]
  test_set <- morphii_toy[testID, ]
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
OUTboost
mean(OUTboost) #0.785
sd(OUTboost) #.014
table(TRUTH,OUTPUT) 
807/(807+102) #sens: 0.876
55/(55+36) #spec: 0.524

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
Sys.time() - st
OUTsvm
mean(OUTsvm) #0.855
sd(OUTsvm) #.015
table(TRUTH,OUTPUT) 
794/(794+96) #sens: 0.892
61/(61+49) #spec: 0.555

###plots

#k-fold class plots
library(colorspace)
namez <- c("Log","LDA","QDA","KNN","Tree","Bag","RF","Boost","SVM")
accs <- c(.843 , .853 , .843 , .85 , .793 , .837 , .85 , .785,.855)
sensitivity <- c(.906 , .947 , 1.00 , .982 , .883 , .864 ,.860,.876,.892)
specificity <- c(.511 , .533 , 0.00 , .267 , .393 , .455,.595,.524,.555)
plot1dat <- rbind(accs,sensitivity,specificity)
barplot(plot1dat,beside = TRUE,col=sequential_hcl(3, "Sunset"),
       main="Accuracy, Sensitivity, and Specificity for K-Fold CV",
      names=namez,xlab="Method")
#barplot(plot1dat,beside = TRUE,col=sequential_hcl(3, "Sunset"),
 #       main="Accuracy, Sensitivity, and Specificity for K-Fold CV",
  #      names=namez)

ses <- c(.020,.028,.015,.019,.024,.019,.023,.014,.015)
barplot(ses,col=sequential_hcl(9, "Sunset"),
        main="Standard Error of Classifiers for K-Fold CV",
        xlab="Method",
        ylab="Standard Error",names=namez)

##classification data
lraccs <- c(0.830,0.870,0.835,0.830,0.865)
ldaaccs <- c(0.840,0.880,0.840,0.820,0.885)
qdaaccs <- c(0.860,0.845,0.840,0.820,0.850)
knnaccs <- c(0.850,0.860,0.840,0.825,0.875)
treeaccs <- c(0.800,0.770,0.765,0.810,0.820)
bagaccs <- c(0.820,0.855,0.840,0.815,0.855)
rfaccs <- c(0.855,0.865,0.840,0.815,0.875)
boostaccs <- c(0.77125,0.79750,0.78125,0.80125,0.77125)
svmaccs <- c(0.835,0.870,0.855,0.845,0.870)
allaccs <- c(lraccs,ldaaccs,qdaaccs,knnaccs,treeaccs,bagaccs,
             rfaccs,boostaccs,svmaccs)
boxplot(lraccs,ldaaccs,qdaaccs,knnaccs,treeaccs,bagaccs,rfaccs,boostaccs,
        svmaccs,
        col=rainbow(9),
        main="Spread of Accuracy for Each Classifier for K-Fold CV",
        xlab = "Method",ylab="Accuracy",names=namez)

#LOOCV plot

accs2 <- c( .847,.855,.846,.853,.793,.842,.84,.782,.846)
sensitivity2 <- c(.851,.940,.843,.846,.868,.862,.854,1,.885)
specificity2 <- c(.172,.599,.981,.108,.316,.491,.444,0,.514)
plot2dat <- rbind(accs2,sensitivity2,specificity2)
barplot(plot2dat,beside = TRUE,col=sequential_hcl(3,"sunset"),
        main="Accuracy, Sensitivity, and Specificity for LOOCV",
        names=namez)

##regression plots
mses <- c(112.118,118.834,118.738,328.527,106.025,19.103,20.567,61.222,113.445)
regnames <- c("Linear","Quad","Cubic","KNN","Tree","Bag","RF","Boost","SVM")
barplot(mses,names=regnames,col=rainbow(8),xlab="Method",ylab="MSE",
        main="MSE for Different Regression Techniques",cex.names = .95)
