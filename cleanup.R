# clean training data


trainingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

testingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(trainingUrl,"./training.csv")
download.file(testingUrl,"./testing.csv")

training <- read.csv("./training.csv")
testing <- read.csv("./testing.csv")

names(training)[160]
names(testing)

class(training$pr)

colNames = c("user_name", "new_window", "num_window", "roll_belt", "pitch_belt", 
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

testingReduced <- subset(testing,select = colNames)

# add 'classe' for training set
trainingReduced <- subset(training,select = c( "classe", colNames))

