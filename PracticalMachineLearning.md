---
title: "Practical Machine Learning"
author: "PY Chia"
date: "Monday, February 23, 2015"
output: html_document
---

## Load Data , Clean Data and Partition

Training data is downloaded from https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

Test data is loaded from https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


```r
library(caret)
set.seed(13333)
pmlTrain<-read.csv("pml-training.csv", header=T, na.strings=c("NA", "#DIV/0!"))
pmlTest<-read.csv("pml-testing.csv", header=T, na.string=c("NA", "#DIV/0!"))
```

## Data set is cleaned and partitioned

Partitioning was performed to obtain a 75% training set and a 25% test set.


```r
## Exclude NAs
noNApmlTrain<-pmlTrain[, apply(pmlTrain, 2, function(x) !any(is.na(x)))] 

## Exclude time and user information
pmlTrain1<-noNApmlTrain[,-c(1:8)]
pmlTest1<-pmlTest[,names(pmlTrain1[,-52])]

#data cleaning, Partitioning was performed to obtain a 75% training set and a 25% test set.

inTrain<-createDataPartition(y=pmlTrain1$classe, p=0.75,list=F)
training<-pmlTrain1[inTrain,] 
test<-pmlTrain1[-inTrain,] 

#Training and test set dimensions
dim(training)
```

```
## [1] 14718    52
```

```r
dim(test)
```

```
## [1] 4904   52
```

## Using ML algorithms for prediction: Random Forest

Random forest trees were generated for the training dataset using cross-validation. The generated algorithm was later examined under the partitioned training set to check the accuracy and estimated error.


```r
fitControl2<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
rffit<-train(classe~.,data=training, method="rf", trControl=fitControl2, verbose=F)
```

```
## + Fold1: mtry= 2 
## - Fold1: mtry= 2 
## + Fold1: mtry=26 
## - Fold1: mtry=26 
## + Fold1: mtry=51 
## - Fold1: mtry=51 
## + Fold2: mtry= 2 
## - Fold2: mtry= 2 
## + Fold2: mtry=26 
## - Fold2: mtry=26 
## + Fold2: mtry=51 
## - Fold2: mtry=51 
## + Fold3: mtry= 2 
## - Fold3: mtry= 2 
## + Fold3: mtry=26 
## - Fold3: mtry=26 
## + Fold3: mtry=51 
## - Fold3: mtry=51 
## + Fold4: mtry= 2 
## - Fold4: mtry= 2 
## + Fold4: mtry=26 
## - Fold4: mtry=26 
## + Fold4: mtry=51 
## - Fold4: mtry=51 
## + Fold5: mtry= 2 
## - Fold5: mtry= 2 
## + Fold5: mtry=26 
## - Fold5: mtry=26 
## + Fold5: mtry=51 
## - Fold5: mtry=51 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 26 on full training set
```

```r
predrf<-predict(rffit, newdata=test)
confusionMatrix(predrf, test$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1391    7    0    0    0
##          B    3  940    4    0    0
##          C    0    0  849    1    1
##          D    0    1    2  802    1
##          E    1    1    0    1  899
## 
## Overall Statistics
##                                         
##                Accuracy : 0.9953        
##                  95% CI : (0.993, 0.997)
##     No Information Rate : 0.2845        
##     P-Value [Acc > NIR] : < 2.2e-16     
##                                         
##                   Kappa : 0.9941        
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9971   0.9905   0.9930   0.9975   0.9978
## Specificity            0.9980   0.9982   0.9995   0.9990   0.9993
## Pos Pred Value         0.9950   0.9926   0.9976   0.9950   0.9967
## Neg Pred Value         0.9989   0.9977   0.9985   0.9995   0.9995
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2836   0.1917   0.1731   0.1635   0.1833
## Detection Prevalence   0.2851   0.1931   0.1735   0.1644   0.1839
## Balanced Accuracy      0.9976   0.9944   0.9962   0.9983   0.9985
```

```r
pred20<-predict(rffit, newdata=pmlTest1)
# Output for the prediction of the 20 cases provided
pred20
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

## Write to Output file

The results of the 20 predictions were then written to the output files.


```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pred20)
```

