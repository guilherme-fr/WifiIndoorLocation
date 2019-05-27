library(ggplot2)
library(dplyr)

buildingFloors <- function(wifi_data, building_id) {
  tempData <- filter(wifi_data, BUILDINGID == building_id)
  ggplot(tempData, aes(x = LONGITUDE, y = LATITUDE, colour = factor(RELATIVEPOSITION))) +
    geom_point() +
    facet_wrap(. ~ FLOOR, ncol = 2)
}

predictedPoints <- function(predictedPoints, validation_set) {
  tempData <- validation_set %>% 
    mutate(LATITUDE_P = predictedPoints$LATITUDE,
           LONGITUDE_P = predictedPoints$LONGITUDE)
  
  tempPredicted <- tempData %>% 
    select(LATITUDE_P, LONGITUDE_P, FLOOR) %>% 
    dplyr::rename(LATITUDE = LATITUDE_P, LONGITUDE = LONGITUDE_P) %>% 
    mutate(PREDICTED = TRUE)
  
  tempActual <- tempData %>% select(LATITUDE, LONGITUDE, FLOOR) %>% mutate(PREDICTED = FALSE)
  
  data <- rbind(tempActual, tempPredicted)
  
  ggplot(data, aes(x = LONGITUDE, y = LATITUDE, colour = PREDICTED)) +
    geom_point() +
    facet_wrap(. ~ FLOOR, ncol = 2)
}

predictedPointsErrors <- function(predictedPoints, validation_set, errors) {
  tempData <- validation_set %>% 
    mutate(LATITUDE_P = predictedPoints$LATITUDE,
           LONGITUDE_P = predictedPoints$LONGITUDE)
  
  predictedData <- tempData %>% 
    select(LATITUDE_P, LONGITUDE_P, FLOOR) %>% 
    mutate(ERROR = errors$absolute_error)
  
  ggplot(predictedData, aes(x = LONGITUDE_P, y = LATITUDE_P, colour = ERROR)) +
    geom_point() +
    facet_wrap(. ~ FLOOR, ncol = 2) +
    scale_color_gradient(low = "blue", high = "red")
}

#' Calculates the following error metrics: absolute error, Mean Absolute Error, relative error, 
#' Mean Relative Error, squared error, root mean squared error
errorMetrics <- function(predictedValues, actualValues) {
  absolute_error <- abs( predictedValues - actualValues )
  MAE <- mean(absolute_error, na.rm = TRUE)
  
  relative_error <- abs( absolute_error / actualValues )
  #Removing the possible 'infinite' values due to actualValue = 0
  relative_error[is.infinite(relative_error)] <- NA
  MRE <- mean(relative_error, na.rm = TRUE)
  
  square_error <- absolute_error * absolute_error
  RMSE <- sqrt( mean(square_error, na.rm = TRUE) )
  
  errors <- data.frame(absolute_error, MAE, relative_error, MRE, square_error, RMSE)
  errors
}