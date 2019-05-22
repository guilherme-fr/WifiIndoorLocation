pacman::p_load(dplyr, caret, randomForest, e1071)
source("scripts/pre_processing.R")
source("scripts/training_functions.R")

####Loading data to memory####
#Loading the training Wifi dataset
wifi_dataset_original <- read.csv("dataset/trainingData.csv")

#Loading the validation Wifi dataset
wifi_validation_original <- read.csv("dataset/validationData.csv")

####Preparing the data set for models#####
no_wap_value <- -105
wifi_data <- wifi_dataset_original

#Replacing the "No wAP signal" values
wifi_data <- replaceNoWAPValues(wifi_data, no_wap_value)

#Removing columns that have low variance (if any)
low_var_cols_index <- lowVarianceCol(wifi_data, 2)
if (length(low_var_cols_index) != 0) {
  wifi_data <- wifi_data[, -low_var_cols_index]
}

#Sampling the data
set.seed(123)

##Sample for BUILDINGS model
wifi_data_buildings <- wifi_data %>% group_by(BUILDINGID, FLOOR) %>% sample_n(10)

#Removing columns with low variance (if any) after sampling
low_var_cols_index2 <- lowVarianceCol(wifi_data_buildings, 2)
if (length(low_var_cols_index2) != 0) {
  wifi_data_buildings <- wifi_data_buildings[, -low_var_cols_index2]
}

##Sample for FLOOR model
#Filter for buildings
wifi_data_floors_b0 <- wifi_data %>% filter(BUILDINGID == 0)
wifi_data_floors_b1 <- wifi_data %>% filter(BUILDINGID == 1)
wifi_data_floors_b2 <- wifi_data %>% filter(BUILDINGID == 2)

#Filter for low variance (if any) for each building data
low_var_cols_floor_b0_index <- lowVarianceCol(wifi_data_floors_b0, 2)
if (length(low_var_cols_floor_b0_index) != 0) {
  wifi_data_floors_b0 <- wifi_data_floors_b0[, -low_var_cols_floor_b0_index]
}

low_var_cols_floor_b1_index <- lowVarianceCol(wifi_data_floors_b1, 7)
if (length(low_var_cols_floor_b1_index) != 0) {
  wifi_data_floors_b1 <- wifi_data_floors_b1[, -low_var_cols_floor_b1_index]
}

low_var_cols_floor_b2_index <- lowVarianceCol(wifi_data_floors_b2, 2)
if (length(low_var_cols_floor_b2_index) != 0) {
  wifi_data_floors_b2 <- wifi_data_floors_b2[, -low_var_cols_floor_b2_index]
}

#Sampling floors for each building
wifi_data_floors_b0 <- wifi_data_floors_b0 %>% 
  group_by(FLOOR, SPACEID, RELATIVEPOSITION) %>% 
  sample_n(ifelse(n() < 5, n(), 5)) #Sample at most 5 points per location

wifi_data_floors_b1 <- wifi_data_floors_b1 %>% 
  group_by(FLOOR, SPACEID, RELATIVEPOSITION) %>% 
  sample_n(ifelse(n() < 5, n(), 5)) #Sample at most 5 points per location

wifi_data_floors_b2 <- wifi_data_floors_b2 %>% 
  group_by(FLOOR, SPACEID, RELATIVEPOSITION) %>% 
  sample_n(ifelse(n() < 5, n(), 5)) #Sample at most 5 points per location

#Removing columns with low variance (if any) after sampling
low_var_cols_floor_b0_index2 <- lowVarianceCol(wifi_data_floors_b0, 1)
if (length(low_var_cols_floor_b0_index2) != 0) {
  wifi_data_floors_b0 <- wifi_data_floors_b0[, -low_var_cols_floor_b0_index2]
}

low_var_cols_floor_b1_index2 <- lowVarianceCol(wifi_data_floors_b1, 1)
if (length(low_var_cols_floor_b1_index2) != 0) {
  wifi_data_floors_b1 <- wifi_data_floors_b1[, -low_var_cols_floor_b1_index2]
}

low_var_cols_floor_b2_index2 <- lowVarianceCol(wifi_data_floors_b2, 1)
if (length(low_var_cols_floor_b2_index2) != 0) {
  wifi_data_floors_b2 <- wifi_data_floors_b2[, -low_var_cols_floor_b2_index2]
}

#Creating the final dataset for Building location prediction
wap_data_buildings <- wifi_data_buildings[, wapColIndex(wifi_data_buildings)]
training_data_building <- cbind(wap_data_buildings, BUILDINGID = as.factor(wifi_data_buildings$BUILDINGID))

#Creating the final datasets for Floor location prediction for each building
wap_data_floors_b0 <- wifi_data_floors_b0[, wapColIndex(wifi_data_floors_b0)]
wap_data_floors_b1 <- wifi_data_floors_b1[, wapColIndex(wifi_data_floors_b1)]
wap_data_floors_b2 <- wifi_data_floors_b2[, wapColIndex(wifi_data_floors_b2)]
training_data_floor_b0 <- cbind(wap_data_floors_b0, FLOOR = as.factor(wifi_data_floors_b0$FLOOR))
training_data_floor_b1 <- cbind(wap_data_floors_b1, FLOOR = as.factor(wifi_data_floors_b1$FLOOR))
training_data_floor_b2 <- cbind(wap_data_floors_b2, FLOOR = as.factor(wifi_data_floors_b2$FLOOR))

####Training models#####
#Creating training and test sets for building predictions
data_building <- createTrainAndTestSets(training_data_building, 
                                        training_data_building$BUILDINGID, 
                                        0.7, 
                                        123)

#Creating training and test sets for Floor predictions for each building
data_floor_b0 <- createTrainAndTestSets(training_data_floor_b0, 
                                        training_data_floor_b0$FLOOR,
                                        0.7, 
                                        123)
data_floor_b1 <- createTrainAndTestSets(training_data_floor_b1, 
                                        training_data_floor_b1$FLOOR,
                                        0.7, 
                                        123)
data_floor_b2 <- createTrainAndTestSets(training_data_floor_b2, 
                                        training_data_floor_b2$FLOOR,
                                        0.7, 
                                        123)

#Training the building prediction model
number_predictors_building <- ncol(data_building$training) - 1

#Random Forest - Buildings
# model_building <- randomForest(y = data_building$training$BUILDINGID,
#                                x = data_building$training[, 1:number_predictors_building],
#                                importance = T,
#                                method = "rf",
#                                ntree=500,
#                                mtry=2)

#K-NN - Buildings
# model_building <- knn3(BUILDINGID ~ ., data = data_building$training, k = 3)

#SVM - Buildings
model_building <- svm(formula = BUILDINGID ~ .,
                      data = training_data_building,
                      kernel = "radial",
                      cost = 10000,
                      gamma = 2.980232e-08)


#Training the floor prediction model for each building
number_predictors_fb0 <- ncol(data_floor_b0$training) - 1
number_predictors_fb1 <- ncol(data_floor_b1$training) - 1
number_predictors_fb2 <- ncol(data_floor_b2$training) - 1
#Random Forest - B0
model_floor_b0 <- randomForest(y = data_floor_b0$training$FLOOR,
                               x = data_floor_b0$training[, 1:number_predictors_fb0],
                               importance = T,
                               method = "rf",
                               ntree = 600,
                               mtry = 13)

#K-NN - B0
# model_floor_b0 <- knn3(FLOOR ~ ., data = data_floor_b0$training, k = 3)

#SVM - B0
# model_floor_b0 <- svm(formula = FLOOR ~ ., 
#                       data = data_floor_b0$training,
#                       kernel = "radial",
#                       cost = 10,
#                       gamma = 0.001953125)

#Random Forest - B1
# model_floor_b1 <- randomForest(y = data_floor_b1$training$FLOOR,
#                                x = data_floor_b1$training[, 1:number_predictors_fb1],
#                                importance = T,
#                                method = "rf",
#                                ntree = 500,
#                                mtry = 11)

#K-NN - B1
# model_floor_b1 <- knn3(FLOOR ~ ., data = data_floor_b1$training, k = 3)

#SVM - B1
model_floor_b1 <- svm(formula = FLOOR ~ ., 
                      data = training_data_floor_b1,
                      kernel = "radial",
                      cost = 1,
                      gamma = 0.00390625)

#Random Forest - B2
model_floor_b2 <- randomForest(y = training_data_floor_b2$FLOOR,
                               x = training_data_floor_b2[, 1:number_predictors_fb2],
                               importance = T,
                               method = "rf",
                               ntree = 500,
                               mtry = 14)

#K-NN - B2
# model_floor_b2 <- knn3(FLOOR ~ ., data = data_floor_b2$training, k = 3)

#SVM - B2
# model_floor_b2 <- svm(formula = FLOOR ~ ., 
#                       data = data_floor_b2$training,
#                       kernel = "radial",
#                       cost = 10,
#                       gamma = 0.0009765625)

####Predictions####
#Predicting the building in test set created
predictions_building <- predict(model_building, data_building$testing, type = "class")

#Predictions the floor in test set created for each building
predictions_floor_b0 <- predict(model_floor_b0, data_floor_b0$testing, type = "class")
predictions_floor_b1 <- predict(model_floor_b1, data_floor_b1$testing, type = "class")
predictions_floor_b2 <- predict(model_floor_b2, data_floor_b2$testing, type = "class")

#Prepare the validation set for prediction
validation_set <- replaceNoWAPValues(wifi_validation_original, no_wap_value)
wapNames <- colnames(wifi_data_buildings[, wapColIndex(wifi_data_buildings)])

#For building
validation_set_building <- validation_set[, wapNames]
validation_set_building <- cbind(validation_set_building, BUILDINGID = as.factor(wifi_validation_original$BUILDINGID))

#For floor
wapNamesB0 <- colnames(wifi_data_floors_b0[, wapColIndex(wifi_data_floors_b0)])
wapNamesB1 <- colnames(wifi_data_floors_b1[, wapColIndex(wifi_data_floors_b1)])
wapNamesB2 <- colnames(wifi_data_floors_b2[, wapColIndex(wifi_data_floors_b2)])
validation_set_floor_b0 <- validation_set %>% filter(BUILDINGID == 0)
validation_set_floor_b1 <- validation_set %>% filter(BUILDINGID == 1)
validation_set_floor_b2 <- validation_set %>% filter(BUILDINGID == 2)
validation_set_floor_b0 <- cbind(validation_set_floor_b0[, wapNamesB0], 
                                 FLOOR = as.factor(validation_set_floor_b0$FLOOR))
validation_set_floor_b1 <- cbind(validation_set_floor_b1[, wapNamesB1], 
                                 FLOOR = as.factor(validation_set_floor_b1$FLOOR))
validation_set_floor_b2 <- cbind(validation_set_floor_b2[, wapNamesB2], 
                                 FLOOR = as.factor(validation_set_floor_b2$FLOOR))

#Predicting building in validation set
predictions_validation_buildings <- predict(model_building, validation_set_building, type = "class")

#Predicting floor in validation set for each building
predictions_validation_floor_b0 <- predict(model_floor_b0, validation_set_floor_b0, type = "class")
predictions_validation_floor_b1 <- predict(model_floor_b1, validation_set_floor_b1, type = "class")
predictions_validation_floor_b2 <- predict(model_floor_b2, validation_set_floor_b2, type = "class")

####Error Analysis####
#For building predictions
assess_model_building <- postResample(predictions_building, data_building$testing$BUILDINGID)
assess_model_validation_building <- postResample(predictions_validation_buildings, 
                                                 validation_set_building$BUILDINGID)

#For floor predictions
assess_model_floor_b0 <- postResample(predictions_floor_b0, data_floor_b0$testing$FLOOR)
assess_model_floor_b1 <- postResample(predictions_floor_b1, data_floor_b1$testing$FLOOR)
assess_model_floor_b2 <- postResample(predictions_floor_b2, data_floor_b2$testing$FLOOR)
assess_model_validation_floor_b0 <- postResample(predictions_validation_floor_b0, 
                                                 validation_set_floor_b0$FLOOR)
assess_model_validation_floor_b1 <- postResample(predictions_validation_floor_b1, 
                                                 validation_set_floor_b1$FLOOR)
assess_model_validation_floor_b2 <- postResample(predictions_validation_floor_b2, 
                                                 validation_set_floor_b2$FLOOR)

