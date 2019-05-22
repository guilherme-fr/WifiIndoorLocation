pacman::p_load(dplyr, reshape, caret)

wapNotPresent <- function(wapData) {
  #Apply function to all WAP info columns
  wap_notpresent_index <- apply(wapData, 2, function(wap_data) {
    #Anonymous function that returns a vector of logical values. TRUE if the WAP was captured in some record
    #of the data. FALSE otherwise.
    wap_present_count <- sum(wap_data < 100)
    
    ifelse(wap_present_count > 0, FALSE, TRUE)
  })
  
  allWap <- colnames(wapData)
  result <- allWap[wap_notpresent_index]
  result
}

wapVariance <- function(wapData) {
  wap_variance <- apply(wapData, 2, function(wap_data) {
    var(wap_data)
  })
  
  wap_variance
}

wapStandardDeviation <- function(wapData) {
  wap_sd <- apply(wapData, 2, function(wap_data) {
    sd(wap_data)
  })
  
  wap_sd
}

#' Replace all the +100 values in the WAP columns for a specific value
#' 
#' @param wifiData Data frame with Wifi location data
#' @param replace_value Numeric value for replace the +100 in the given data frame
#' 
#' @return A new data frame with all +100 values replaced by replace_value
replaceNoWAPValues <- function(wifiData, replace_value) {
  wap_col_indexes <- wapColIndex(wifiData)
  result <- wifiData %>%
    mutate_at(wap_col_indexes, function(wap_data) {
      ifelse(wap_data == 100, replace_value, wap_data)
    })
  
  result
}

#' Return the columns that present low variance regarding a given threshold
#' 
#' @param wapData Data frama with the WAP signal strength
#' @param var_cutoff Min value of variance allowed
#' 
#' @return A vector with the index of all columns that have low variance
#' 
lowVarianceCol <- function(wifiData, var_cutoff) {
  wap_col_indexes <- wapColIndex(wifiData)
  wap_var <- wapVariance(wifiData[, wap_col_indexes])
  which(wap_var < var_cutoff)
}

wapColIndex <- function(wifiData) {
  grep("WAP", colnames(wifiData))
}