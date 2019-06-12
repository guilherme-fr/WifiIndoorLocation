pacman::p_load(dplyr, caret, randomForest, e1071, BBmisc)
source("scripts/pre_processing.R")
source("scripts/utils.R")

####Files####
#File for test
file_test_set <- "dataset/testData.csv"

#Files for saved models
file_model_building <- "models/model_building.rda"

file_model_floor_b0 <- "models/model_floor_b0.rda"
file_model_floor_b1 <- "models/model_floor_b1.rda"
file_model_floor_b2 <- "models/model_floor_b2.rda"

file_model_lat_b0 <- "models/model_lat_b0.rda"
file_model_lat_b1 <- "models/model_lat_b1.rda"
file_model_lat_b2 <- "models/model_lat_b2.rda"

file_model_lon_b0 <- "models/model_lon_b0.rda"
file_model_lon_b1 <- "models/model_lon_b1.rda"
file_model_lon_b2 <- "models/model_lon_b2.rda"

#Files for WAPs for each model
file_wap_building <- "models/wap_building.csv"

file_wap_floor_b0 <- "models/wap_floor_b0.csv"
file_wap_floor_b1 <- "models/wap_floor_b1.csv"
file_wap_floor_b2 <- "models/wap_floor_b2.csv"

file_wap_lat_b0 <- "models/wap_lat_b0.csv"
file_wap_lat_b1 <- "models/wap_lat_b1.csv"
file_wap_lat_b2 <- "models/wap_lat_b2.csv"

file_wap_lon_b0 <- "models/wap_lon_b0.csv"
file_wap_lon_b1 <- "models/wap_lon_b1.csv"
file_wap_lon_b2 <- "models/wap_lon_b2.csv"

####Constants####
no_wap_value <- -105
normalize_data <- FALSE

####Loading objects from file system####
#Loading models
model_building <- readRDS(file = file_model_building)

model_floor_b0 <- readRDS(file = file_model_floor_b0)
model_floor_b1 <- readRDS(file = file_model_floor_b1)
model_floor_b2 <- readRDS(file = file_model_floor_b2)

model_lat_b0 <- readRDS(file = file_model_lat_b0)
model_lat_b1 <- readRDS(file = file_model_lat_b1)
model_lat_b2 <- readRDS(file = file_model_lat_b2)

model_lon_b0 <- readRDS(file = file_model_lon_b0)
model_lon_b1 <- readRDS(file = file_model_lon_b1)
model_lon_b2 <- readRDS(file = file_model_lon_b2)

#Loading WAP names for each model
wap_names_building <- read.csv(file = file_wap_building, stringsAsFactors = FALSE)

wap_names_floor_b0 <- read.csv(file = file_wap_floor_b0, stringsAsFactors = FALSE)
wap_names_floor_b1 <- read.csv(file = file_wap_floor_b1, stringsAsFactors = FALSE)
wap_names_floor_b2 <- read.csv(file = file_wap_floor_b2, stringsAsFactors = FALSE)

wap_names_lat_b0 <- read.csv(file = file_wap_lat_b0, stringsAsFactors = FALSE)
wap_names_lat_b1 <- read.csv(file = file_wap_lat_b1, stringsAsFactors = FALSE)
wap_names_lat_b2 <- read.csv(file = file_wap_lat_b2, stringsAsFactors = FALSE)

wap_names_lon_b0 <- read.csv(file = file_wap_lon_b0, stringsAsFactors = FALSE)
wap_names_lon_b1 <- read.csv(file = file_wap_lon_b1, stringsAsFactors = FALSE)
wap_names_lon_b2 <- read.csv(file = file_wap_lon_b2, stringsAsFactors = FALSE)

#Loading test set
test_set_original <- read.csv(file_test_set)

####Prepare data for building prediction####
test_set <- replaceNoWAPValues(test_set_original, no_wap_value)
#Creating Index column to merge the different data set at the end
test_set <- test_set %>% mutate(ID = 1:nrow(test_set_original))

#Preparing data for building prediction
wap_set_building <- test_set[, wap_names_building[, 1] ]

####Building prediction####
predictions_bulding <- predict(model_building, wap_set_building, type = "class")

####Prepare data for floors, latitude and longitude predictions####
test_set_b0 <- test_set %>% filter(predictions_bulding == 0)
test_set_b1 <- test_set %>% filter(predictions_bulding == 1)
test_set_b2 <- test_set %>% filter(predictions_bulding == 2)

#WAPs for floors
wap_set_floor_b0 <- test_set_b0[, wap_names_floor_b0[, 1] ]
wap_set_floor_b1 <- test_set_b1[, wap_names_floor_b1[, 1] ]
wap_set_floor_b2 <- test_set_b2[, wap_names_floor_b2[, 1] ]

#WAPs for latitude
wap_set_lat_b0 <- test_set_b0[, wap_names_lat_b0[, 1] ]
wap_set_lat_b1 <- test_set_b1[, wap_names_lat_b1[, 1] ]
wap_set_lat_b2 <- test_set_b2[, wap_names_lat_b2[, 1] ]

#WAPs for longitude
wap_set_lon_b0 <- test_set_b0[, wap_names_lon_b0[, 1] ]
wap_set_lon_b1 <- test_set_b1[, wap_names_lon_b1[, 1] ]
wap_set_lon_b2 <- test_set_b2[, wap_names_lon_b2[, 1] ]

####Normalize the data by row####
if (normalize_data) {
  #Floors
  wap_set_floor_b0 <- normalize(x = wap_set_floor_b0, method = "range", margin = 1)
  for(wap in colnames(wap_set_floor_b0)) {
    if (sum(wap_set_floor_b0[, wap] != 0.5) == 0) {
      wap_set_floor_b0[, wap] <- 0
    }
  }
  wap_set_floor_b1 <- normalize(x = wap_set_floor_b1, method = "range", margin = 1)
  for(wap in colnames(wap_set_floor_b1)) {
    if (sum(wap_set_floor_b1[, wap] != 0.5) == 0) {
      wap_set_floor_b1[, wap] <- 0
    }
  }
  wap_set_floor_b2 <- normalize(x = wap_set_floor_b2, method = "range", margin = 1)
  for(wap in colnames(wap_set_floor_b2)) {
    if (sum(wap_set_floor_b2[, wap] != 0.5) == 0) {
      wap_set_floor_b2[, wap] <- 0
    }
  }
  
  #Latitude
  wap_set_lat_b0 <- normalize(x = wap_set_lat_b0, method = "range", margin = 1)
  for(wap in colnames(wap_set_lat_b0)) {
    if (sum(wap_set_lat_b0[, wap] != 0.5) == 0) {
      wap_set_lat_b0[, wap] <- 0
    }
  }
  wap_set_lat_b1 <- normalize(x = wap_set_lat_b1, method = "range", margin = 1)
  for(wap in colnames(wap_set_lat_b1)) {
    if (sum(wap_set_lat_b1[, wap] != 0.5) == 0) {
      wap_set_lat_b1[, wap] <- 0
    }
  }
  wap_set_lat_b2 <- normalize(x = wap_set_lat_b2, method = "range", margin = 1)
  for(wap in colnames(wap_set_lat_b2)) {
    if (sum(wap_set_lat_b2[, wap] != 0.5) == 0) {
      wap_set_lat_b2[, wap] <- 0
    }
  }
  
  #Longitude
  wap_set_lon_b0 <- normalize(x = wap_set_lon_b0, method = "range", margin = 1)
  for(wap in colnames(wap_set_lon_b0)) {
    if (sum(wap_set_lon_b0[, wap] != 0.5) == 0) {
      wap_set_lon_b0[, wap] <- 0
    }
  }
  wap_set_lon_b1 <- normalize(x = wap_set_lon_b1, method = "range", margin = 1)
  for(wap in colnames(wap_set_lon_b1)) {
    if (sum(wap_set_lon_b1[, wap] != 0.5) == 0) {
      wap_set_lon_b1[, wap] <- 0
    }
  }
  wap_set_lon_b2 <- normalize(x = wap_set_lon_b2, method = "range", margin = 1)
  for(wap in colnames(wap_set_lon_b2)) {
    if (sum(wap_set_lon_b2[, wap] != 0.5) == 0) {
      wap_set_lon_b2[, wap] <- 0
    }
  }
}

####Floor predictions####
predictions_floor_b0 <- predict(model_floor_b0, wap_set_floor_b0, type = "class")
predictions_floor_b1 <- predict(model_floor_b1, wap_set_floor_b1, type = "class")
predictions_floor_b2 <- predict(model_floor_b2, wap_set_floor_b2, type = "class")

#####Latitude predictions####
predictions_lat_b0 <- predict(model_lat_b0, wap_set_lat_b0)
predictions_lat_b1 <- predict(model_lat_b1, wap_set_lat_b1)
predictions_lat_b2 <- predict(model_lat_b2, wap_set_lat_b2)

####Longitude predictions####
predictions_lon_b0 <- predict(model_lon_b0, wap_set_lon_b0)
predictions_lon_b1 <- predict(model_lon_b1, wap_set_lon_b1)
predictions_lon_b2 <- predict(model_lon_b2, wap_set_lon_b2)

####Preparing output file####
#Preparing Floor column
floor_b0_temp <- test_set_b0 %>% select(ID) %>% mutate(FLOOR = predictions_floor_b0)
floor_b1_temp <- test_set_b1 %>% select(ID) %>% mutate(FLOOR = predictions_floor_b1)
floor_b2_temp <- test_set_b2 %>% select(ID) %>% mutate(FLOOR = predictions_floor_b2)

result_floors <- rbind(floor_b0_temp, floor_b1_temp, floor_b2_temp)

#Preparing Latitude column
lat_b0_temp <- test_set_b0 %>% select(ID) %>% mutate(LATITUDE = predictions_lat_b0)
lat_b1_temp <- test_set_b1 %>% select(ID) %>% mutate(LATITUDE = predictions_lat_b1)
lat_b2_temp <- test_set_b2 %>% select(ID) %>% mutate(LATITUDE = predictions_lat_b2)

result_lat <- rbind(lat_b0_temp, lat_b1_temp, lat_b2_temp)

#Preparing Longitude column
lon_b0_temp <- test_set_b0 %>% select(ID) %>% mutate(LONGITUDE = predictions_lon_b0)
lon_b1_temp <- test_set_b1 %>% select(ID) %>% mutate(LONGITUDE = predictions_lon_b1)
lon_b2_temp <- test_set_b2 %>% select(ID) %>% mutate(LONGITUDE = predictions_lon_b2)

result_lon <- rbind(lon_b0_temp, lon_b1_temp, lon_b2_temp)

#Preparing Building column
result_building <- test_set %>% select(ID) %>% mutate(BUILDINGID = predictions_bulding)

#Preparing final result
result <- result_lat %>%
  inner_join(result_lon, by = "ID") %>%
  inner_join(result_floors, by = "ID") %>%
  arrange(ID) %>%
  mutate(ID = NULL)

####Saving the final result in file system####
write.csv(result, file = "dataset/test_result.csv", row.names = FALSE, quote = FALSE)

#Errors measures based on IPIN2015 competition
calculate_error <- FALSE
if (calculate_error) {
  building_penalty <- 50
  floor_penalty <- 4
  
  result_building <- result_building %>% arrange(ID)
  result_floors <- result_floors %>% arrange(ID)
  result_lat <- result_lat %>% arrange(ID)
  result_lon <- result_lon %>% arrange(ID)
  
  building_error <- building_penalty * ifelse(result_building$BUILDINGID == test_set_original$BUILDINGID, 0, 1)
  floor_error <- floor_penalty * ifelse(result_floors$FLOOR == test_set_original$FLOOR, 0, 1)
  # floor_error <- floor_penalty * abs(result_floors$FLOOR - test_set_original$FLOOR)
  coord_error <- sqrt((result_lat$LATITUDE - test_set_original$LATITUDE)^2 + 
                        (result_lon$LONGITUDE - test_set_original$LONGITUDE)^2)
  
  error_vector <- building_error + floor_error + coord_error
  error <- mean(error_vector)
}

