library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)

# Download the training data
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
              destfile = "./pml-training.csv", method = "curl")

# Load the training dataset
dt_training <- read.csv("./pml-training.csv", na.strings=c("NA","#DIV/0!",""))

# Download the testing data
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
              destfile = "./pml-testing.csv", method = "curl")

# Load the testing dataset
dt_testing <- read.csv("./pml-training.csv", na.strings=c("NA","#DIV/0!",""))

# Partitioning the Training Set

# Partition the training set in two data sets, 60% training and 40% testing
in_train <- createDataPartition(y = dt_training$classe, p=0.6, list=FALSE)
training <- dt_training[in_train, ]
testing <- dt_training[-in_train, ]

dim(training) 
dim(testing)


# Cleaning the Data
nzv_data <- nearZeroVar(training, saveMetrics=TRUE)

nzv_vars <- names(training) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
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

training <- training[!nzv_vars]
training <- training[c(-1)]

training2 <- training

for(i in 1:length(training)) { 
    if( sum( is.na(training[, i] ) ) /nrow(training) >= .6 ) {
        for(j in 1:length(training2)) {
            if( length( grep(names(training[i]), names(training2)[j]) ) ==1)  { 
                training2 <- training2[ , -j]
            }   
        } 
    }
}

training <- training2
rm(training2)

clean1 <- colnames(training)
clean2 <- colnames(training[, -58])
testing <- testing[clean1]
testing <- testing[clean2]

for (i in 1:length(testing) ) {
    for(j in 1:length(training)) {
        if( length( grep(names(training[i]), names(testing)[j]) ) ==1)  {
            class(testing[j]) <- class(training[i])
        }      
    }      
}

testing <- rbind(training[2, -58] , testing)
testing <- testing[-1,]

set.seed(12345)

modFit <- rpart(classe ~ ., data = training, method="class")
fancyRpartPlot(modFit)

prediction <- predict(modFit, testing, type = "class")
confusionMatrix(prediction, testing$classe)

modFit <- randomForest(classe ~. , data = training)

prediction <- predict(modFit, testing, type = "class")

confusionMatrix(prediction, testing$classe)

pml_write_files = function(x) {
    n = length(x)

    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(prediction)