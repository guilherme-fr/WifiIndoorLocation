library(ggplot2)
library(dplyr)

buildingFloors <- function(wifi_data, building_id) {
  tempData <- filter(wifi_data, BUILDINGID == building_id)
  ggplot(tempData, aes(x = LONGITUDE, y = LATITUDE, colour = factor(RELATIVEPOSITION))) +
    geom_point() +
    facet_wrap(. ~ FLOOR, ncol = 2)
}