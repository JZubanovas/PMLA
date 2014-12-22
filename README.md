#Practical Machine Learning Assignment

 Installing the necessary packages
Error: unexpected symbol in "Installing the"
 
 ```r

 library(caret)
 library(rattle)
 library(randomForest)
 ```

 Reading the CSV files and clensing the data
Error: unexpected symbol in "Reading the"
 ```r

 training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
 testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
 ```

 Splitting the ```training``` data set in to training and test sets
Error: unexpected symbol in "Splitting the"
 ```r

 set.seed(456)
 inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
 myTraining <- training[inTrain, ]; myTesting <- training[-inTrain, ]
 ```

 Cleaning NearZeroVariance Variables
Error: unexpected symbol in "Cleaning NearZeroVariance"
 ```r

 myDataNZV <- nearZeroVar(myTraining, saveMetrics=TRUE)
 ```

 Creating another subset without NZV variables
Error: unexpected symbol in "Creating another"
 ```r

 myNZVvars <- names(myTraining) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
 "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
 "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
 "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
 "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
 "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
 "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
 "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
 "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
 "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
 "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
 "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
 "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
 "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
 "stddev_yaw_forearm", "var_yaw_forearm")
 
 myTraining <- myTraining[!myNZVvars]
 ```

 Excluding the first column of the data set, which contains the ID number
Error: unexpected symbol in "Excluding the"
 ```r

 myTraining <- myTraining[c(-1)]
 ```

 
 The training set still contains a lot of columns with NA values. We will exlude columns with more that 60% data as NA
Error: unexpected symbol in "The training"
 ```r

 trainingV3 <- myTraining
 for(i in 1:length(myTraining)) {
         if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) = .6 ) {
         for(j in 1:length(trainingV3)) {
             if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
                 trainingV3 <- trainingV3[ , -j] #Remove that column
             }   
         } 
     }
 }
 ```

 Overwriting the initial object
Error: unexpected symbol in "Overwriting the"
 ```r

 myTraining <- trainingV3
 rm(trainingV3)
 ```

 Now repeat the same procedure for ```myTesting``` and ```testind``` sets
Error: unexpected 'repeat' in "Now repeat"
 ```r

 clean1 <- colnames(myTraining)
 clean2 <- colnames(myTraining[, -58]) #already with classe column removed
 myTesting <- myTesting[clean1]
 testing <- testing[clean2]
 ```

 Applying the randonm forest technique on the training set
Error: unexpected symbol in "Applying the"
 ```r

 set.seed(470)
 modFitB1 <- randomForest(classe ~. , data=myTraining)
 predictionsB1 <- predict(modFitB1, myTraining, type = "class")
 confusionMatrix(predictionsB1, myTraining$classe)
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 3348    0    0    0    0
         B    0 2279    0    0    0
         C    0    0 2054    0    0
         D    0    0    0 1930    0
         E    0    0    0    0 2165

Overall Statistics
                                     
               Accuracy : 1          
                 95% CI : (0.9997, 1)
    No Information Rate : 0.2843     
    P-Value [Acc  NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
 Mcnemar's Test P-Value : NA         

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
 ```

 The CI for the Accuracy is high, we will apply the same predictors to the test set, expecting at least 0.9997 accuracy with a 95% confidence
Error: unexpected symbol in "The CI"
 ```r

 predictionsB1 <- predict(modFitB1, myTesting, type = "class")
 confusionMatrix(predictionsB1, myTesting$classe)
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 2232    1    0    0    0
         B    0 1517    1    0    0
         C    0    0 1367    3    0
         D    0    0    0 1282    1
         E    0    0    0    1 1441

Overall Statistics
                                          
               Accuracy : 0.9991          
                 95% CI : (0.9982, 0.9996)
    No Information Rate : 0.2845          
    P-Value [Acc  NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9989          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   0.9993   0.9993   0.9969   0.9993
Specificity            0.9998   0.9998   0.9995   0.9998   0.9998
Pos Pred Value         0.9996   0.9993   0.9978   0.9992   0.9993
Neg Pred Value         1.0000   0.9998   0.9998   0.9994   0.9998
Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2845   0.1933   0.1742   0.1634   0.1837
Detection Prevalence   0.2846   0.1935   0.1746   0.1635   0.1838
Balanced Accuracy      0.9999   0.9996   0.9994   0.9984   0.9996
 ```

