library(colorspace)
regnames <- c("Linear","KNN","Tree","Bag","RF","Boost","SVM")
namez <- c("Log","LDA","QDA","KNN","Tree","Bag","RF","Boost","SVM")

##scratch
mses <- c(112.118,118.834,118.738,328.527,106.025,19.103,20.567,
          61.222,113.445)
barplot(mse,col=sequential_hcl(7,"sunset"),
        main="MSE for Different Regression Techniques",
        names=regnames,xlab="Method",ylab="MSE")


##kfold pca

accs <- c(.894,.919,.843,.857,.814,.848,.843,.877,.881)
sens <- c(.935,.940,1.000,.876,.879,.849,1.000,.877,.923)
spec <- c(.667,.784,.000,.603,.391,.778,.000,.870,.623)
stde <- c(.017,.016,.016,.016,.024,.033,.027,.004,.015)
plotdat <- rbind(accs,sens,spec)
barplot(plotdat,beside = TRUE,col=sequential_hcl(3,"sunset"),
        main="Accuracy, Sensitivity, and Specificity for\n K-Fold CV With PCA",
        names=namez,xlab="Method")
barplot(accs,col=sequential_hcl(9,"sunset"),
        main="Accuracy for K-Fold CV With PCA",
        names=namez,xlab="Method")
barplot(stde,col=sequential_hcl(9,"sunset"),
        main="Std. Error for K-Fold CV With PCA",
        names=namez,xlab="Method",ylab="Standard Error")

##pca regression

mse <- c(86.216,207.955,190.825,161.050,174.008,101.604,86.339)
barplot(mse,col=sequential_hcl(7,"sunset"),
        main="MSE for Age Regression with PCA",
        names=regnames,xlab="Method",ylab="MSE")

##kfold kpca
accs <- c(.884,.916,.843,.849,.786,.851,.843,.868,.843)
sens <- c(.932,.938,1.000,.867,.873,.851,1.000,.868,1.000)
spec <- c(.629,.774,.000,.547,.318,.833,.000,.857,.000)
stde <- c(.027,.011,.027,.015,.034,.029,.027,.009,.027)
plotdat <- rbind(accs,sens,spec)
barplot(plotdat,beside = TRUE,col=sequential_hcl(3,"sunset"),
        main="Accuracy, Sensitivity, and Specificity for\n K-Fold CV With KPCA",
        names=namez,xlab="Method")
barplot(accs,col=sequential_hcl(9,"sunset"),
        main="Accuracy for K-Fold CV With KPCA",
        names=namez,xlab="Method")
barplot(stde,col=sequential_hcl(9,"sunset"),
        main="Std. Error for K-Fold CV With KPCA",
        names=namez,xlab="Method",ylab="Standard Error")

##regression kpca
mse <- c(126.24,462.060,155.190,114.544,111.943,130.581,128.505)
barplot(mse,col=sequential_hcl(7,"sunset"),
        main="MSE for Age Regression with KPCA",
        names=regnames,xlab="Method",ylab="MSE")
