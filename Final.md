# Practical Maching Learning Course Project

# Loading train and test dataset

```r
require(caret)
```

```
## Loading required package: caret
```

```
## Warning: package 'caret' was built under R version 3.1.3
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
ptrain <- read.csv("pml-training.csv")
ptest <- read.csv("pml-testing.csv")
```

## Partition training data set into training and validation datasets to estimate out-of-sample error

```r
set.seed(10)
inTrain <- createDataPartition(y= ptrain$classe, p = .7, list = FALSE)
ptrain1 <- ptrain[inTrain, ]
ptrain2 <- ptrain[-inTrain, ]
```

## Reduce variables 

```r
# Reduce number of features by removing variables with nearly zero variance i.e. variables that are almost always NA

# remove variables with nearly zero variance
nzv <- nearZeroVar(ptrain1)
ptrain1 <- ptrain1[, -nzv]
ptrain2 <- ptrain2[, -nzv]

# remove variables that are almost always NA
mostlyNA <- sapply(ptrain1, function(x) mean(is.na(x))) > 0.95
ptrain1 <- ptrain1[, mostlyNA==F]
ptrain2 <- ptrain2[, mostlyNA==F]

# remove miscellaneous variables that are not useful for prediction 
#(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp) --> 1st 5 vars
ptrain1 <- ptrain1[, -(1:5)]
ptrain2 <- ptrain2[, -(1:5)]
```

## Build the Model

```r
# Use Random Forest model. Fit the model on small training set (ptrain1), and instruct the "train" function to use 3-fold cross-validation to select optimal tuning parameters for the model.

# instruct train to use 3-fold CV to select optimal tuning parameters
fitControl <- trainControl(method="cv", number=3, verboseIter=F)

# fit model on ptrain1
fit <- train(classe ~ ., data=ptrain1, method="rf", trControl=fitControl)
```

```
## Loading required package: randomForest
```

```
## Warning: package 'randomForest' was built under R version 3.1.3
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

## Print Model Results

```r
# print final model to see tuning parameters it chose
fit$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 27
## 
##         OOB estimate of  error rate: 0.23%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3904    1    0    0    1 0.0005120328
## B    5 2649    4    0    0 0.0033860045
## C    0    5 2391    0    0 0.0020868114
## D    0    0    8 2243    1 0.0039964476
## E    0    0    0    6 2519 0.0023762376
```
Results: 500 trees are used with 27 variables at each split.

## Model Evaluation and Selection

```r
# Use the fitted model to predict the label ("classe") in the other training set (ptrain2), and show the confusion matrix to compare the predicted versus the actual labels:

# use model to predict classe in validation set (ptrain2)
preds <- predict(fit, newdata=ptrain2)

# show confusion matrix to get estimate of out-of-sample error
confusionMatrix(ptrain2$classe, preds)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    0    0    0    0
##          B    3 1134    1    1    0
##          C    0    2 1024    0    0
##          D    0    0    2  962    0
##          E    0    0    0    2 1080
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9981          
##                  95% CI : (0.9967, 0.9991)
##     No Information Rate : 0.285           
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9976          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9982   0.9982   0.9971   0.9969   1.0000
## Specificity            1.0000   0.9989   0.9996   0.9996   0.9996
## Pos Pred Value         1.0000   0.9956   0.9981   0.9979   0.9982
## Neg Pred Value         0.9993   0.9996   0.9994   0.9994   1.0000
## Prevalence             0.2850   0.1930   0.1745   0.1640   0.1835
## Detection Rate         0.2845   0.1927   0.1740   0.1635   0.1835
## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
## Balanced Accuracy      0.9991   0.9986   0.9983   0.9982   0.9998
```
Results: The accuracy is 99.8%, thus predicted accuracy for the out-of-sample error is 0.2%. Random Forest will be used to do prediction.

## Training using Selected Model

```r
# Train on the original training set
# remove variables with nearly zero variance
nzv <- nearZeroVar(ptrain)
ptrain <- ptrain[, -nzv]
ptest <- ptest[, -nzv]

# remove variables that are almost always NA
mostlyNA <- sapply(ptrain, function(x) mean(is.na(x))) > 0.95
ptrain <- ptrain[, mostlyNA==F]
ptest <- ptest[, mostlyNA==F]

# remove variables that don't make intuitive sense for prediction (X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp), which happen to be the first five variables
ptrain <- ptrain[, -(1:5)]
ptest <- ptest[, -(1:5)]

# re-fit model using full training set (ptrain)
fitControl <- trainControl(method="cv", number=3, verboseIter=F)
fit <- train(classe ~ ., data=ptrain, method="rf", trControl=fitControl)
```

## Making Predictions

```r
# predict on test set
preds <- predict(fit, newdata=ptest)

# convert predictions to character vector
preds <- as.character(preds)

# create function to write predictions to files
pml_write_files <- function(x) {
    n <- length(x)
    for(i in 1:n) {
        filename <- paste0("problem_id_", i, ".txt")
        write.table(x[i], file=filename, quote=F, row.names=F, col.names=F)
    }
}

# create prediction files to submit
pml_write_files(preds)
```
