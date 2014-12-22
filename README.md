##Coursera - Practical Machine Learning - Assessment
 
Installing the necessary packages and setting the required options

```{r}
library(caret)
library(rattle)
library(randomForest)
library(knitr)
opts_chunk$set(warning = FALSE, message = FALSE, echo = TRUE, cache = TRUE, 
               fig.width = 10, fig.height = 10)
```
Data Processing, cleansing and splitting
```{r}
training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
 ```
Splitting the  training  data set in to training and test sets
```{r} 
set.seed(456)
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
Training <- training[inTrain, ]; Testing <- training[-inTrain, ]
```
Cleaning NearZeroVariance Variables
```{r} 
DataNZV <- nearZeroVar(Training, saveMetrics=TRUE)
 ```
Creating another subset without NZV variables
```{r}
NZVvars <- names(Training) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
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

Training <- Training[!NZVvars]
``` 
Excluding the first column of the data set, which contains the ID number
```{r}
Training <- Training[c(-1)]
 
```
The training set still contains a lot of columns with NA values. We will exlude columns with more that 60% data as NA
```{r}
TrainingTemp <- Training
for(i in 1:length(Training)) {
        if( sum( is.na( Training[, i] ) ) /nrow(Training) >= .6 ) {
        for(j in 1:length(TrainingTemp)) {
            if( length( grep(names(Training[i]), names(TrainingTemp)[j]) ) ==1)  { #if the columns are the same:
                TrainingTemp <- TrainingTemp[ , -j] #Remove that column
            }   
        } 
    }
}
```
Overwriting the initial object
```{r}
Training <- TrainingTemp
rm(TrainingTemp)
```
Now repeat the same procedure for  ```Testing```  and  ```testing```  sets
```{r}
clean1 <- colnames(Training)
clean2 <- colnames(Training[, -58]) #already with classe column removed
Testing <- Testing[clean1]
testing <- testing[clean2]
```
Applying the randonm forest technique on the training set
```{r}
set.seed(470)
modFit <- randomForest(classe ~. , data=Training, importance=TRUE, keep.forest=TRUE)
print(modFit)
```
**There is no need for cross-validation as it is is estimated internally by the OOB error estimate ```(0.12%)```**
```{r}
predictionsV1 <- predict(modFit, Training, type = "class")
 

confusionMatrix(predictionsV1, Training$classe)
 

predictionsV2 <- predict(modFit, Testing, type = "class")
confusionMatrix(predictionsV2, Testing$classe)
```
The model proved to be accurate on the ```Testing``` set, with a ```0.999``` accuracy.
