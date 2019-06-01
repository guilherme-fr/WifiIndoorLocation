pacman::p_load(dplyr, caret, randomForest, e1071)
source("scripts/pre_processing.R")
source("scripts/training_functions.R")
source("scripts/plots.R")
source("scripts/utils.R")

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

# wifi_data_floors_b0 <- wifi_data_floors_b0 %>% 
#   filter_at(vars(wapColIndex(wifi_data_floors_b0)), all_vars(. <= -25))
# wifi_data_floors_b1 <- wifi_data_floors_b1 %>% 
#   filter_at(vars(wapColIndex(wifi_data_floors_b1)), all_vars(. <= -25))
# wifi_data_floors_b2 <- wifi_data_floors_b2 %>% 
#   filter_at(vars(wapColIndex(wifi_data_floors_b2)), all_vars(. <= -25))


# wifi_data_floors_b1 <- wifi_data_floors_b1 %>% 
#   filter(ifelse(FLOOR == 2, RELATIVEPOSITION != 1, TRUE))



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

#Normalize data
# wifi_data_floors_b0[, wapColIndex(wifi_data_floors_b0)] <- normalize(
#   x = wifi_data_floors_b0[, wapColIndex(wifi_data_floors_b0)],
#   method = "range",
#   margin = 1)
# wifi_data_floors_b1[, wapColIndex(wifi_data_floors_b1)] <- normalize(
#   x = wifi_data_floors_b1[, wapColIndex(wifi_data_floors_b1)],
#   method = "range",
#   margin = 1)
# wifi_data_floors_b2[, wapColIndex(wifi_data_floors_b2)] <- normalize(
#   x = wifi_data_floors_b2[, wapColIndex(wifi_data_floors_b2)],
#   method = "range",
#   margin = 1)



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

#Creating the final datasets for LATITUDE prediction
training_data_lat_b0 <- cbind(wap_data_floors_b0, LATITUDE = wifi_data_floors_b0$LATITUDE)
training_data_lat_b1 <- cbind(wap_data_floors_b1, LATITUDE = wifi_data_floors_b1$LATITUDE)
training_data_lat_b2 <- cbind(wap_data_floors_b2, LATITUDE = wifi_data_floors_b2$LATITUDE)

#Creating the final datasets for LONGITUDE prediction
training_data_lon_b0 <- cbind(wap_data_floors_b0, LONGITUDE = wifi_data_floors_b0$LONGITUDE)
training_data_lon_b1 <- cbind(wap_data_floors_b1, LONGITUDE = wifi_data_floors_b1$LONGITUDE)
training_data_lon_b2 <- cbind(wap_data_floors_b2, LONGITUDE = wifi_data_floors_b2$LONGITUDE)

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

#Creating training and test sets for LATITUDE predictions for each building
data_lat_b0 <- createTrainAndTestSets(training_data_lat_b0, 
                                        training_data_lat_b0$LATITUDE,
                                        0.7, 
                                        123)
data_lat_b1 <- createTrainAndTestSets(training_data_lat_b1, 
                                      training_data_lat_b1$LATITUDE,
                                      0.7, 
                                      123)
data_lat_b2 <- createTrainAndTestSets(training_data_lat_b2, 
                                      training_data_lat_b2$LATITUDE,
                                      0.7, 
                                      123)

#Creating training and test sets for LONGITUDE predictions for each building
data_lon_b0 <- createTrainAndTestSets(training_data_lon_b0, 
                                      training_data_lon_b0$LONGITUDE,
                                      0.7, 
                                      123)
data_lon_b1 <- createTrainAndTestSets(training_data_lon_b1, 
                                      training_data_lon_b1$LONGITUDE,
                                      0.7, 
                                      123)
data_lon_b2 <- createTrainAndTestSets(training_data_lon_b2, 
                                      training_data_lon_b2$LONGITUDE,
                                      0.7, 
                                      123)


####Training the building prediction model#####
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


#####Training the floor prediction model for each building####
number_predictors_fb0 <- ncol(data_floor_b0$training) - 1
number_predictors_fb1 <- ncol(data_floor_b1$training) - 1
number_predictors_fb2 <- ncol(data_floor_b2$training) - 1
#Random Forest - B0
# model_floor_b0 <- randomForest(y = data_floor_b0$training$FLOOR,
#                                x = data_floor_b0$training[, 1:number_predictors_fb0],
#                                importance = T,
#                                method = "rf",
#                                ntree = 600,
#                                mtry = 13)
model_floor_b0 <- randomForest(y = training_data_floor_b0$FLOOR,
                               x = training_data_floor_b0[, 1:number_predictors_fb0],
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
# model_floor_b2 <- randomForest(y = data_floor_b2$training$FLOOR,
#                                x = data_floor_b2$training[, 1:number_predictors_fb2],
#                                importance = T,
#                                method = "rf",
#                                ntree = 500,
#                                mtry = 14)
# model_floor_b2 <- randomForest(y = training_data_floor_b2$FLOOR,
#                                x = training_data_floor_b2[, 1:number_predictors_fb2],
#                                importance = T,
#                                method = "rf",
#                                ntree = 500,
#                                mtry = 14)

#K-NN - B2
# model_floor_b2 <- knn3(FLOOR ~ ., data = data_floor_b2$training, k = 3)

#SVM - B2
# model_floor_b2 <- svm(formula = FLOOR ~ ., 
#                       data = data_floor_b2$training,
#                       kernel = "radial",
#                       cost = 10,
#                       gamma = 0.0009765625)


####Training the LATITUDE prediction model for each building####
number_predictors_latb0 <- ncol(data_lat_b0$training) - 1
number_predictors_latb1 <- ncol(data_lat_b1$training) - 1
number_predictors_latb2 <- ncol(data_lat_b2$training) - 1
#Random Forest - B0
# model_lat_b0 <- randomForest(y = data_lat_b0$training$LATITUDE,
#                              x = data_lat_b0$training[, 1:number_predictors_latb0],
#                              importance = T,
#                              method = "rf",
#                              ntree = 500,
#                              mtry = 18)

#K-NN - B0
# model_lat_b0 <- knnreg(LATITUDE ~ ., data = data_lat_b0$training, k = 5)
model_lat_b0 <- knnreg(LATITUDE ~ ., data = training_data_lat_b0, k = 5)

#SVM - B0
# model_lat_b0 <- svm(formula = LATITUDE ~ .,
#                       data = data_lat_b0$training,
#                       kernel = "radial",
#                       cost = 10000,
#                       gamma = 4.768372e-07)

#Random Forest - B1
# model_lat_b1 <- randomForest(y = data_lat_b1$training$LATITUDE,
#                              x = data_lat_b1$training[, 1:number_predictors_latb1],
#                              importance = T,
#                              method = "rf",
#                              ntree = 500,
#                              mtry = 20)
model_lat_b1 <- randomForest(y = training_data_lat_b1$LATITUDE,
                             x = training_data_lat_b1[, 1:number_predictors_latb1],
                             importance = T,
                             method = "rf",
                             ntree = 500,
                             mtry = 20)

#K-NN - B1
# model_lat_b1 <- knnreg(LATITUDE ~ ., data = data_lat_b1$training, k = 5)

#SVM - B1
# model_lat_b1 <- svm(formula = LATITUDE ~ .,
#                       data = data_lat_b1$training,
#                       kernel = "radial",
#                       cost = 10000,
#                       gamma = 2.384186e-07)

#Random Forest - B2
# model_lat_b2 <- randomForest(y = data_lat_b2$training$LATITUDE,
#                              x = data_lat_b2$training[, 1:number_predictors_latb2],
#                              importance = T,
#                              method = "rf",
#                              ntree = 500,
#                              mtry = 20)

#K-NN - B2
# model_lat_b2 <- knnreg(LATITUDE ~ ., data = data_lat_b2$training, k = 11)
model_lat_b2 <- knnreg(LATITUDE ~ ., data = training_data_lat_b2, k = 11)

#SVM - B2
# model_lat_b2 <- svm(formula = LATITUDE ~ .,
#                     data = data_lat_b2$training,
#                     kernel = "radial",
#                     cost = 1000,
#                     gamma = 9.536743e-07)



#####Training the LONGITUDE prediction model for each building####
number_predictors_lonb0 <- ncol(data_lon_b0$training) - 1
number_predictors_lonb1 <- ncol(data_lon_b1$training) - 1
number_predictors_lonb2 <- ncol(data_lon_b2$training) - 1
#Random Forest - B0
# model_lon_b0 <- randomForest(y = data_lon_b0$training$LONGITUDE,
#                              x = data_lon_b0$training[, 1:number_predictors_lonb0],
#                              importance = T,
#                              method = "rf",
#                              ntree = 500,
#                              mtry = 15)

#K-NN - B0
# model_lon_b0 <- knnreg(LONGITUDE ~ ., data = data_lon_b0$training, k = 9)
model_lon_b0 <- knnreg(LONGITUDE ~ ., data = training_data_lon_b0, k = 9)

#SVM - B0
# model_lon_b0 <- svm(formula = LONGITUDE ~ .,
#                       data = data_lon_b0$training,
#                       kernel = "radial",
#                       cost = 100000,
#                       gamma = 9.536743e-07)

#Random Forest - B1
# model_lon_b1 <- randomForest(y = data_lon_b1$training$LONGITUDE,
#                              x = data_lon_b1$training[, 1:number_predictors_lonb1],
#                              importance = T,
#                              method = "rf",
#                              ntree = 500,
#                              mtry = 20)
model_lon_b1 <- randomForest(y = training_data_lon_b1$LONGITUDE,
                             x = training_data_lon_b1[, 1:number_predictors_lonb1],
                             importance = T,
                             method = "rf",
                             ntree = 500,
                             mtry = 20)

#K-NN - B1
# model_lon_b1 <- knnreg(LONGITUDE ~ ., data = data_lon_b1$training, k = 7)

#SVM - B1
# model_lon_b1 <- svm(formula = LONGITUDE ~ .,
#                       data = data_lon_b1$training,
#                       kernel = "radial",
#                       cost = 10000,
#                       gamma = 4.768372e-07)

#Random Forest - B2
# model_lon_b2 <- randomForest(y = data_lon_b2$training$LONGITUDE,
#                              x = data_lon_b2$training[, 1:number_predictors_lonb2],
#                              importance = T,
#                              method = "rf",
#                              ntree = 500,
#                              mtry = 18)

#K-NN - B2
# model_lon_b2 <- knnreg(LONGITUDE ~ ., data = data_lon_b2$training, k = 11)
model_lon_b2 <- knnreg(LONGITUDE ~ ., data = training_data_lon_b2, k = 11)

#SVM - B2
# model_lon_b2 <- svm(formula = LONGITUDE ~ .,
#                       data = data_lon_b2$training,
#                       kernel = "radial",
#                       cost = 100000,
#                       gamma = 1.490116e-08)


####Predictions####
#Predicting the building in test set created
predictions_building <- predict(model_building, data_building$testing, type = "class")

#Predictions the floor in test set created for each building
predictions_floor_b0 <- predict(model_floor_b0, data_floor_b0$testing, type = "class")
predictions_floor_b1 <- predict(model_floor_b1, data_floor_b1$testing, type = "class")
predictions_floor_b2 <- predict(model_floor_b2, data_floor_b2$testing, type = "class")

#Predictions the LATITUDE in test set created for each building
predictions_lat_b0 <- predict(model_lat_b0, data_lat_b0$testing)
predictions_lat_b1 <- predict(model_lat_b1, data_lat_b1$testing)
predictions_lat_b2 <- predict(model_lat_b2, data_lat_b2$testing)

#Predictions the LONGITUDE in test set created for each building
predictions_lon_b0 <- predict(model_lon_b0, data_lon_b0$testing)
predictions_lon_b1 <- predict(model_lon_b1, data_lon_b1$testing)
predictions_lon_b2 <- predict(model_lon_b2, data_lon_b2$testing)

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
validation_set_floor_b0_temp <- validation_set %>% filter(BUILDINGID == 0)
validation_set_floor_b1_temp <- validation_set %>% filter(BUILDINGID == 1)
validation_set_floor_b2_temp <- validation_set %>% filter(BUILDINGID == 2)


#Normalize the validation set
# validation_set_floor_b0_temp[, wapNamesB0] <- normalize(
#   x = validation_set_floor_b0_temp[, wapNamesB0],
#   method = "range",
#   margin = 1)
# validation_set_floor_b1_temp[, wapNamesB1] <- normalize(
#   x = validation_set_floor_b1_temp[, wapNamesB1],
#   method = "range",
#   margin = 1)
# validation_set_floor_b2_temp[, wapNamesB2] <- normalize(
#   x = validation_set_floor_b2_temp[, wapNamesB2],
#   method = "range",
#   margin = 1)
# 
# #Replace constant values by 0
# for(wap in wapNamesB0) {
#   if (sum(validation_set_floor_b0_temp[, wap] != 0.5) == 0) {
#     validation_set_floor_b0_temp[, wap] <- 0
#   }
# }
# for(wap in wapNamesB1) {
#   if (sum(validation_set_floor_b1_temp[, wap] != 0.5) == 0) {
#     validation_set_floor_b1_temp[, wap] <- 0
#   }
# }
# for(wap in wapNamesB2) {
#   if (sum(validation_set_floor_b2_temp[, wap] != 0.5) == 0) {
#     validation_set_floor_b2_temp[, wap] <- 0
#   }
# }



validation_set_floor_b0 <- cbind(validation_set_floor_b0_temp[, wapNamesB0], 
                                 FLOOR = as.factor(validation_set_floor_b0_temp$FLOOR))
validation_set_floor_b1 <- cbind(validation_set_floor_b1_temp[, wapNamesB1], 
                                 FLOOR = as.factor(validation_set_floor_b1_temp$FLOOR))
validation_set_floor_b2 <- cbind(validation_set_floor_b2_temp[, wapNamesB2], 
                                 FLOOR = as.factor(validation_set_floor_b2_temp$FLOOR))

#For LATITUDE
validation_set_lat_b0 <- cbind(validation_set_floor_b0_temp[, wapNamesB0], 
                                 LATITUDE = validation_set_floor_b0_temp$LATITUDE)
validation_set_lat_b1 <- cbind(validation_set_floor_b1_temp[, wapNamesB1], 
                               LATITUDE = validation_set_floor_b1_temp$LATITUDE)
validation_set_lat_b2 <- cbind(validation_set_floor_b2_temp[, wapNamesB2], 
                               LATITUDE = validation_set_floor_b2_temp$LATITUDE)

#For LONGITUDE
validation_set_lon_b0 <- cbind(validation_set_floor_b0_temp[, wapNamesB0], 
                               LONGITUDE = validation_set_floor_b0_temp$LONGITUDE)
validation_set_lon_b1 <- cbind(validation_set_floor_b1_temp[, wapNamesB1], 
                               LONGITUDE = validation_set_floor_b1_temp$LONGITUDE)
validation_set_lon_b2 <- cbind(validation_set_floor_b2_temp[, wapNamesB2], 
                               LONGITUDE = validation_set_floor_b2_temp$LONGITUDE)

#Predicting building in validation set
predictions_validation_buildings <- predict(model_building, validation_set_building, type = "class")

#Predicting floor in validation set for each building
predictions_validation_floor_b0 <- predict(model_floor_b0, validation_set_floor_b0, type = "class")
predictions_validation_floor_b1 <- predict(model_floor_b1, validation_set_floor_b1, type = "class")
predictions_validation_floor_b2 <- predict(model_floor_b2, validation_set_floor_b2, type = "class")

#Predicting LATITUDE in validation set for each building
predictions_validation_lat_b0 <- predict(model_lat_b0, validation_set_lat_b0)
predictions_validation_lat_b1 <- predict(model_lat_b1, validation_set_lat_b1)
predictions_validation_lat_b2 <- predict(model_lat_b2, validation_set_lat_b2)

#Predicting LONGITUDE in validation set for each building
predictions_validation_lon_b0 <- predict(model_lon_b0, validation_set_lon_b0)
predictions_validation_lon_b1 <- predict(model_lon_b1, validation_set_lon_b1)
predictions_validation_lon_b2 <- predict(model_lon_b2, validation_set_lon_b2)


####Adjustments in predictions####
#Adjusments based on LATITUDE and LONGITUDE predicted
# b1_points <- data.frame(LATITUDE = predictions_validation_lat_b1,
#                         LONGITUDE = predictions_validation_lon_b1)
# building_sector <- buildingSector(b1_points)
# for(i in 1:nrow(validation_set_floor_b1)) {
#   if (building_sector$Building[i] == 1 && building_sector$Sector[i] == 1) {
#     #Building 1, Sector 1 (middle)
#     if (predictions_validation_floor_b1[i] == 0) {
#       #For sector 1 of building 1, when the model predicts floor = 0, it is more likely to be
#       #floor = 1
#       predictions_validation_floor_b1[i] <- 1
#     }
#   }
# }



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

#For LATITUDE predictions
assess_model_lat_b0 <- postResample(predictions_lat_b0, data_lat_b0$testing$LATITUDE)
assess_model_lat_b1 <- postResample(predictions_lat_b1, data_lat_b1$testing$LATITUDE)
assess_model_lat_b2 <- postResample(predictions_lat_b2, data_lat_b2$testing$LATITUDE)
assess_model_validation_lat_b0 <- postResample(predictions_validation_lat_b0, 
                                                 validation_set_lat_b0$LATITUDE)
errors_model_validation_lat_b0 <- errorMetrics(predictions_validation_lat_b0, validation_set_lat_b0$LATITUDE)
assess_model_validation_lat_b1 <- postResample(predictions_validation_lat_b1, 
                                                 validation_set_lat_b1$LATITUDE)
errors_model_validation_lat_b1 <- errorMetrics(predictions_validation_lat_b1, validation_set_lat_b1$LATITUDE)
assess_model_validation_lat_b2 <- postResample(predictions_validation_lat_b2, 
                                                 validation_set_lat_b2$LATITUDE)
errors_model_validation_lat_b2 <- errorMetrics(predictions_validation_lat_b2, validation_set_lat_b2$LATITUDE)

#For LONGITUDE predictions
assess_model_lon_b0 <- postResample(predictions_lon_b0, data_lon_b0$testing$LONGITUDE)
assess_model_lon_b1 <- postResample(predictions_lon_b1, data_lon_b1$testing$LONGITUDE)
assess_model_lon_b2 <- postResample(predictions_lon_b2, data_lon_b2$testing$LONGITUDE)
assess_model_validation_lon_b0 <- postResample(predictions_validation_lon_b0, 
                                               validation_set_lon_b0$LONGITUDE)
errors_model_validation_lon_b0 <- errorMetrics(predictions_validation_lon_b0, validation_set_lon_b0$LONGITUDE)
assess_model_validation_lon_b1 <- postResample(predictions_validation_lon_b1, 
                                               validation_set_lon_b1$LONGITUDE)
errors_model_validation_lon_b1 <- errorMetrics(predictions_validation_lon_b1, validation_set_lon_b1$LONGITUDE)
assess_model_validation_lon_b2 <- postResample(predictions_validation_lon_b2, 
                                               validation_set_lon_b2$LONGITUDE)
errors_model_validation_lon_b2 <- errorMetrics(predictions_validation_lon_b2, validation_set_lon_b2$LONGITUDE)

