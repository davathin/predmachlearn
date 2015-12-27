# How well do you lift weights?
## David C Latshaw II

# Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell from six participants. The six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).

Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes. Participants were supervised by an experienced weight lifter to make sure the execution complied to the manner they were supposed to simulate. The exercises were performed by six male participants aged between 20-28 years, with little weight lifting experience. We made sure that all participants could easily simulate the mistakes in a safe and controlled manner by using a relatively light dumbbell (1.25kg).

# Data Source

The training data for this project are available here: 
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

# Data Prep

First we load the training and test sets.  


```r
library(RCurl)
train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
train_data <- read.csv(text=getURL(train_url), na.strings=c("", "\"\"", "NA"))
test_data <- read.csv(text=getURL(test_url), na.strings=c("", "\"\"", "NA"))
```

Then we will remove the columns that have no bearing on the analysis that we intend to perform. This is user and nime stamp information.  


```r
var_remove <- c("X",
                    "user_name",
                    "raw_timestamp_part_1",
                    "raw_timestamp_part_2",
                    "cvtd_timestamp",
                    "new_window",
                    "num_window")
for (col in var_remove) {
    train_data[, col] <- NULL
}
```

Next we will get rid of any columns that have almost entirely all NA values.


```r
emptys <- apply(train_data,2,function(x) {sum(is.na(x))})
train_data <- train_data[,which(emptys == 0)]
```

After cleaning up the data set we have the final list of predictors to be used in the model.


```r
names(train_data)
```

```
##  [1] "roll_belt"            "pitch_belt"           "yaw_belt"            
##  [4] "total_accel_belt"     "gyros_belt_x"         "gyros_belt_y"        
##  [7] "gyros_belt_z"         "accel_belt_x"         "accel_belt_y"        
## [10] "accel_belt_z"         "magnet_belt_x"        "magnet_belt_y"       
## [13] "magnet_belt_z"        "roll_arm"             "pitch_arm"           
## [16] "yaw_arm"              "total_accel_arm"      "gyros_arm_x"         
## [19] "gyros_arm_y"          "gyros_arm_z"          "accel_arm_x"         
## [22] "accel_arm_y"          "accel_arm_z"          "magnet_arm_x"        
## [25] "magnet_arm_y"         "magnet_arm_z"         "roll_dumbbell"       
## [28] "pitch_dumbbell"       "yaw_dumbbell"         "total_accel_dumbbell"
## [31] "gyros_dumbbell_x"     "gyros_dumbbell_y"     "gyros_dumbbell_z"    
## [34] "accel_dumbbell_x"     "accel_dumbbell_y"     "accel_dumbbell_z"    
## [37] "magnet_dumbbell_x"    "magnet_dumbbell_y"    "magnet_dumbbell_z"   
## [40] "roll_forearm"         "pitch_forearm"        "yaw_forearm"         
## [43] "total_accel_forearm"  "gyros_forearm_x"      "gyros_forearm_y"     
## [46] "gyros_forearm_z"      "accel_forearm_x"      "accel_forearm_y"     
## [49] "accel_forearm_z"      "magnet_forearm_x"     "magnet_forearm_y"    
## [52] "magnet_forearm_z"     "classe"
```

# Random Forest Model

Next we build the random forest model to classify each action. In order to evaluate the accuracy of the model we will split the training data into 80% training and 20% testing to perform 10-fold cross validation.


```r
library(randomForest)
set.seed(0)
obs <- c()
preds <- c()
for(i in 1:10) {
    intrain = sample(1:dim(train_data)[1], size=dim(train_data)[1] * 0.8, replace=F)
    train_crossval = train_data[intrain,]
    test_crossval = train_data[-intrain,]
    modelformula <- "classe ~ ."
    rf <- randomForest(formula=as.formula(modelformula), data=train_crossval)
    obs <- c(obs, test_crossval$classe)
    preds <- c(preds, predict(rf, test_crossval))
    }
```

The confusion matrix for cross validation is:


```r
library(caret)
conf_matrix <- confusionMatrix(test_crossval$classe,predict(rf, test_crossval))
conf_matrix$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 1162    2    0    0    0
##          B    5  733    0    0    0
##          C    0    0  667    0    0
##          D    0    0    2  617    0
##          E    0    0    0    2  735
```

The accuracy is of the model is 99.7197452%, which is quite good so this will be used for the final model. We will now reform the model based on the full data set and apply it.


```r
final_model <- randomForest(classe ~ ., data=train_data)
```
