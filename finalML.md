---
title: "Final ML Project"
author: "Lulu T"
date: "26/05/2021"
output: 
  html_document: 
    keep_md: yes
---




## Download the data 
Download the data and change the outcome variable to a factor variable. 


```r
traindata <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings = c(" ","","#DIV/0!","NA"))
testdata <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", na.strings = c(" ","","#DIV/0!", "NA"))
traindata$classe <- as.factor(traindata$classe)
dim(traindata)
```

```
## [1] 19622   160
```

## Clean up the data:

Get rid of variables that have more that 20% NA values. 

```r
maxNA <- 0.2 * nrow(traindata)
NAcolumns <- which(colSums(is.na(traindata)) > maxNA)
trainfinal <- traindata[,-NAcolumns]
testfinal <- testdata[,-NAcolumns]
dim(trainfinal)
```

```
## [1] 19622    60
```

```r
names(trainfinal)
```

```
##  [1] "X"                    "user_name"            "raw_timestamp_part_1"
##  [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
##  [7] "num_window"           "roll_belt"            "pitch_belt"          
## [10] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
## [13] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
## [16] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
## [19] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
## [22] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
## [25] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
## [28] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
## [31] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
## [34] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
## [37] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [40] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
## [43] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [46] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
## [49] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
## [52] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
## [55] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
## [58] "magnet_forearm_y"     "magnet_forearm_z"     "classe"
```
Can further narrow data set by eliminating variables that have near zero variability or are useless in predicting classe such as time stamp, new_window and user name variables.

```r
removetrain <- grep("timestamp|name|new_window|X", names(trainfinal))
removetest <- grep("timestamp|name|new_window|X|problem_id", names(testfinal))
trainfinal <- trainfinal[,-c(removetrain)]
testfinal <-  testfinal[, -c(removetest)]
dim(trainfinal)
```

```
## [1] 19622    54
```

```r
dim(testfinal)
```

```
## [1] 20 53
```
## Cross validation - Create data partitions, 
- Cross validation was performed by by partitioning our training data set randomly without replacement into 2 sub samples, one for the training set (0.75) and the other for the testing set (0.25). They were partitioned using createDataPartition function.  Then used Boostrap resampling in the training data set for cross validation. 


```r
inTrain = createDataPartition(trainfinal$classe, p = 3/4)[[1]]
training = trainfinal[ inTrain,]  
testing = trainfinal[-inTrain,] 
plot(training$classe, main="Frequency of each Classe in Training Set", xlab="classe", ylab="Frequency")
```

![](finalML_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Variables to include in models
- Tested which variable correlate highly to select the best variables to include in model thereby reducing the noise of the model. Based on the results, we decided to include all the variables in the model. 

```r
corvar <- training[, -54]
M <- abs(cor(corvar))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
```

```
##                  row col
## yaw_belt           4   2
## total_accel_belt   5   2
## accel_belt_y      10   2
## accel_belt_z      11   2
## accel_belt_x       9   3
## magnet_belt_x     12   3
## roll_belt          2   4
## roll_belt          2   5
## accel_belt_y      10   5
## accel_belt_z      11   5
## pitch_belt         3   9
## magnet_belt_x     12   9
## roll_belt          2  10
## total_accel_belt   5  10
## accel_belt_z      11  10
## roll_belt          2  11
## total_accel_belt   5  11
## accel_belt_y      10  11
## pitch_belt         3  12
## accel_belt_x       9  12
## gyros_arm_y       20  19
## gyros_arm_x       19  20
## magnet_arm_x      25  22
## accel_arm_x       22  25
## magnet_arm_z      27  26
## magnet_arm_y      26  27
## accel_dumbbell_x  35  29
## accel_dumbbell_z  37  30
## gyros_dumbbell_z  34  32
## gyros_forearm_z   47  32
## gyros_dumbbell_x  32  34
## gyros_forearm_z   47  34
## pitch_dumbbell    29  35
## yaw_dumbbell      30  37
## gyros_forearm_z   47  46
## gyros_dumbbell_x  32  47
## gyros_dumbbell_z  34  47
## gyros_forearm_y   46  47
```


## Create models:
- We created several models using all the variables from the training data set, to predict classe in the test data set. The accuracy of each model was measured and then each model was used to predict the final test set for this assignment. However the model with the highest accuracy was used in the quiz. The models used are the following: Random forest, SVM and decision trees.These three models are best for classification predictions. 

## The expected out-of-sample error:
- The expected out-of-sample error will correspond to the "accuracy" measure in the cross-validation data of the model used. This will be obtained from the confusionMatrix function that compares the prediction to actual classe in testing data set. "Accuracy" is the proportion of correct classified observation over the total count of sample in the testing data set. We expect the out-of-sample error to be similar to initial testing accuracy measure because we avoided over fitting in our models i.e. reduced bias. 

# First model: Random Forest 


```r
train_control <- trainControl(method="boot", number=100)
mod1 <- randomForest(classe~., data=training, trControl = train_control)
plot(mod1)
```

![](finalML_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
pred1 <- predict(mod1,testing)
accuracyrf <- confusionMatrix(pred1, testing$classe)$overall[1]
accuracyrf
```

```
##  Accuracy 
## 0.9979608
```
- The out of sample error is 1 - Accuracy rate. Which is 0.0018

# Second Model: Decision Tree


```r
mod2 <- train(classe ~ ., method = "rpart", data=training, trControl = train_control)
pred2 <-  predict(mod2, testing)
accuracyrpart <- confusionMatrix(pred2, testing$classe)$overall[1]
accuracyrpart
```

```
##  Accuracy 
## 0.5279364
```


# Third Model: SVM

```r
mod3 <-  svm(classe ~. , data=training, trControl = train_control )
pred3 <- predict(mod3,testing)
accuracysvm <- confusionMatrix(pred3, testing$classe)$overall[1]
```

The accuracy ratings are as follows:
 - for random forest : 0.9979608  
 - for decision tree : 0.5279364  
 - for svm:  0.9506525  

## Predicting the test data set with all three models


```r
predictfinal1 <- predict(mod1, newdata =testfinal)
predictfinal1
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```


```r
predictfinal2 <- predict(mod2, newdata = testfinal)
predictfinal2
```

```
##  [1] C A C A A C C A A A C C C A C A A A A C
## Levels: A B C D E
```


```r
predictfinal3 <- predict(mod3, newdata = testfinal)
predictfinal3
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  A  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

-The first model(Random Forest) predictions were submitted to the quiz, and they showed an accuracy of 100%. 



