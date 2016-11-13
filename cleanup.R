# clean training data

# 
# trainingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
# 
# testingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
# 
# download.file(trainingUrl,"./training.csv")
# download.file(testingUrl,"./testing.csv")

trainingCsv <- read.csv("./training.csv")
testingCsv <- read.csv("./testing.csv")

names(trainingCsv)[160] # classe



colNames = c("roll_belt", "pitch_belt", 
             "yaw_belt", "total_accel_belt", "gyros_belt_x", "gyros_belt_y", 
             "gyros_belt_z", "accel_belt_x", "accel_belt_y", "accel_belt_z", 
             "magnet_belt_x", "magnet_belt_y", "magnet_belt_z", "roll_arm", 
             "pitch_arm", "yaw_arm", "total_accel_arm", "gyros_arm_x", "gyros_arm_y", 
             "gyros_arm_z", "accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x", 
             "magnet_arm_y", "magnet_arm_z", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell",
             "total_accel_dumbbell", "gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z",
             "accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z", "magnet_dumbbell_x",
             "magnet_dumbbell_y", "magnet_dumbbell_z", "roll_forearm", "pitch_forearm", 
             "yaw_forearm", "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z", 
             "accel_forearm_x", "accel_forearm_y", "accel_forearm_z", "magnet_forearm_x", 
             "magnet_forearm_y", "magnet_forearm_z") 
# ,"problem_id")

# save till end of project
testingReduced <- subset(testingCsv,select = colNames)


trainingReduced <- subset(trainingCsv,select = c("classe", colNames))

# Splitting - as per idiom from class notes
library(caret)
inTrain <- createDataPartition(y=trainingReduced$classe, p=0.7, list=FALSE)

training <- trainingReduced[inTrain,]
testing <- trainingReduced[-inTrain,]
dim(training)
dim(testing)

# GLM

library(caret)
set.seed(12345)

# GLM inappropriate - 2-class outcome only
modelGlm <- train(classe ~ ., data=trainingReduced, method="glm")
modelGlm$finalModel$coefficients["classe"]


predictGlm <- predict(modelGlm, newdata = testingReduced)
predictGlm
confusionMatrix(predictGlm, testingReduced$new_window)
# CART classification and regression tree

modelRPart <- train(classe ~ ., data=training, method="rpart")
predictRPart <- predict(modelRPart, newdata = testing)

str(predictRPart)
str(testing$classe)
confusionMatrix(predictRPart, testing$classe)$overall["Accuracy"]

#0.49  

# Random Forest
# this takes forever !!!
# real weakness of R - single threaded

#modelRF <- train(classe ~ ., data=training, method="rf")


# http://stackoverflow.com/questions/15321947/problematic-random-forest-training-runtime-when-using-formula-interface
head(training[,-1])
modelRF <- randomForest(x=training[,-1],y = training$classe, 
                        ntree = 1000, do.trace = T)

#modelRF <- randomForest(x=training[,-1],y = training$classe, 
#                        ntree = 10, do.trace = T, prox=T)
modelRF
predictRF <- predict(modelRF, newdata=testing)

confusionMatrix(predictRF, testing$classe)
confusionMatrix(predictRF, testing$classe)$overall["Accuracy"]
# .9957 !

varImpPlot(modelRF,n.var = 10,pch=9,col="blue")
?varImpPlot
MDSplot(modelRF, testing$classe,pch=20)
?MDSplot
plot(modelRF)
# predict results from the testing.csv file
predict(modelRF, newdata = testingReduced)

#> predict(modelRF, newdata = testingReduced)
#1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
#B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
#Levels: A B C D E
#> 


load(file="final.Rdata")


