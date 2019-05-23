library(ggplot2)
library(dplyr)

buildingFloors <- function(wifi_data, building_id) {
  tempData <- filter(wifi_data, BUILDINGID == building_id)
  ggplot(tempData, aes(x = LONGITUDE, y = LATITUDE, colour = factor(RELATIVEPOSITION))) +
    geom_point() +
    facet_wrap(. ~ FLOOR, ncol = 2)
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