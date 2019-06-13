library(poLCA)
library(colorspace)

#loading and subsetting data
chems <- read.csv("D:/UNCW REU/ChemConvic.csv",header=TRUE)
convict <- chems[,1]
morphii_m <- read.csv("D:/UNCW REU/morphII_cleaned_v2.csv", header=TRUE)
baby <- read.csv("D:/UNCW REU/MorphII_BIF_s7-37_g0.1_max_partial.csv",header=FALSE)
colnames(baby)[colnames(baby)=="V1"] <- "photo"
morphii_m$photo <- substr(morphii_m$photo, 8, 500)
merged <- merge(baby,morphii_m,by="photo",all.x=TRUE)
#fix(merged)
vars <- c(2574,2575,2577)
morphii_dems <- merged[,vars]
morphii_chems <- morphii_dems[seq(1:500),]
#fix(chems)

#comparing demographics
races1 <- table(morphii_dems$race)
races2 <- table(morphii_chems$race)
table(morphii_dems$gender)
table(morphii_chems$gender)
par(mfrow=c(1,2))
barplot(races1,col=sequential_hcl(5,"sunset"),
        main="Races in Full Toy Set",xlab="Race")
barplot(races2,col=sequential_hcl(5,"sunset"),
        main="Races in Chem. Measures Set",xlab="Race")
hist(morphii_dems$age,col="brown",xlab="Age",
     main="Ages in Full Toy Set")
hist(morphii_chems$age,col="gray",xlab="Age",
     main="Ages in Chem. Measures Set")

#heat map correlation matrix
#fix(chems)
library(corrplot)
corrdata <- chems[,seq(2:10)] 
colnames(corrdata) <- c("m1","m2","m3","m4","m5","m6","m7","m8","m9")
corrplot(cor(corrdata),method="color",tl.col="black")
cor(corrdata)

#finding optimal num of groups
medians <- rep(0,9,5)
for (i in 1:9) {
  medians[i] <- median(chems[,i+1])
}
measures <- chems[,-1]
dich <- measures
for (i in 1:9) {
  for (j in 1:500) {
    if (measures[j,i]<medians[i]) {
      dich[j,i] <- 1
    } else {
      dich[j,i] <- 2
    }
  }
}
#fix(dich)

#running LCA
func <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9) ~ 1
set.seed(3)
gof=matrix(1,10,5)
colnames(gof)<-c("llik","aic","bic","Gsq","Chisq")
for (i in 2:10){
  res=poLCA(func,dich,graphs = T,nclass=i,verbose=FALSE)
  gof[i-1,]=c(res$llik,res$aic,res$bic,res$Gsq,res$Chisq)
}
gof #3
set.seed(3)
res <- poLCA(func,dich,graphs = T,nclass=3,verbose=FALSE)
res
classes=res$predclass
table(classes)
postprobs <- round(res$posterior,2)
probsvec <- rep(0,500)
for (i in 1:500){probsvec[i]=postprobs[i,classes[i]]}
summary(probsvec)
hist(probsvec,col="orange",
     main="Histogram of Classification Probabilities",
     xlab="Certainty of Class Placement")
c1dat <- rbind(res$probs$m1[1,],res$probs$m2[1,],res$probs$m3[1,],
               res$probs$m4[1,],res$probs$m5[1,],res$probs$m6[1,],
               res$probs$m7[1,],res$probs$m8[1,],res$probs$m9[1,])
c2dat <- rbind(res$probs$m1[2,],res$probs$m2[2,],res$probs$m3[2,],
               res$probs$m4[2,],res$probs$m5[2,],res$probs$m6[2,],
               res$probs$m7[2,],res$probs$m8[2,],res$probs$m9[2,])
c3dat <- rbind(res$probs$m1[3,],res$probs$m2[3,],res$probs$m3[3,],
               res$probs$m4[3,],res$probs$m5[3,],res$probs$m6[3,],
               res$probs$m7[3,],res$probs$m8[3,],res$probs$m9[3,])
rownames(c1dat)=rownames(c2dat)=rownames(c3dat)=colnames(data)[1:9]
library(grDevices)
barplot(t(c1dat[,c(2,1)]),col=palette(c("orange","white")),
        main="Group 1 Distribution", xlab="Measure",
        names=c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9"),las=2)
barplot(t(c2dat[,c(2,1)]),col=palette(c("orange","white")),
        main="Group 2 Distribution",
        names=c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9"),las=2)
barplot(t(c3dat[,c(2,1)]),col=palette(c("orange","white")),
        main="Group 3 Distribution",
        names=c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9"),las=2)
clas <- res$predclass
withclass <- cbind(morphii_chems,clas)
c1s <- subset(withclass,withclass$clas == 1)
c2s <- subset(withclass,withclass$clas == 2)
c3s <- subset(withclass,withclass$clas == 3)

#examining demogaphics of classes
table(c1s$gender)
table(c2s$gender)
table(c3s$gender)
table(c1s$race)
table(c2s$race)
table(c3s$race)
par(mfrow=c(1,3))
hist(c1s$age,main="Class 1 Ages",xlab="Age",col="orange")
hist(c2s$age,main="Class 2 Ages",col="blue",xlab="Age")
hist(c3s$age,main="Class 3 Ages",col="green",xlab="Age")

#logistic and linear regression
lmfit <- lm(age ~ clas,data=withclass)
summary(lmfit)
lmpred <- predict(lmfit,withclass)
mean((lmpred - withclass$age)^2) #65.149
withcon <- cbind(withclass, convict)
glmfit <- glm(convict ~ clas, data=withcon, family="binomial")
glmfit
glmpred <- predict(glmfit,withcon,type="response")
summary(glmpred)
glmclass <- rep(0,500)
for(i in 1:500) {
  if (glmpred[i] > 0.5) {
    glmclass[i] <- 1
  }
}
mean(glmclass == withcon$convict) #accuracy: 0.764
table(glmclass)
table(withcon$convict)
#sensitivity: 1.000
#specificity: 0.000

#graphs for funsies
table(classes)
fix(withclass)
withchem <- cbind(measures,clas)
c1 <- subset(withchem,clas==1)
c2 <- subset(withchem,clas==2)
c3 <- subset(withchem,clas==3)
corrdata1 <- c1[,seq(1:9)] 
colnames(corrdata1) <- c("m1","m2","m3","m4","m5","m6","m7","m8","m9")
corrplot(cor(corrdata1),method="color",tl.col="black")
cor(corrdata1)

corrdata2 <- c2[,seq(1:9)] 
colnames(corrdata2) <- c("m1","m2","m3","m4","m5","m6","m7","m8","m9")
corrplot(cor(corrdata2),method="color",tl.col="black")
cor(corrdata2)

corrdata3 <- c3[,seq(1:9)] 
colnames(corrdata3) <- c("m1","m2","m3","m4","m5","m6","m7","m8","m9")
corrplot(cor(corrdata3),method="color",tl.col="black")
cor(corrdata3)