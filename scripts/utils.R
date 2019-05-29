library(sp)

buildingSector <- function(points) {
  result_list <- apply(points, 1, function(p) {
    buildingAndSector <- NULL
    if (point.in.polygon(point.x = p["LONGITUDE"],
                     point.y = p["LATITUDE"],
                     pol.x = c(-7490, -7490, -7530, -7530),
                     pol.y = c(4864875, 4864825, 4864825, 4864875)) > 0) {

      buildingAndSector <- data.frame(Building = 1, Sector = 1)
    } else {
      buildingAndSector <- data.frame(Building = -1, Sector = -1)
    }
    
    buildingAndSector
  })
  
  result_dataframe <- data.frame(matrix(unlist(result_list), nrow = length(result_list), byrow = TRUE))
  colnames(result_dataframe) <- c("Building", "Sector")
  
  result_dataframe
}

distanceToLine <- function(point, line.point1, line.point2) {
  v1 <- line.point1 - line.point2
  v2 <- point - line.point1
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
  d
}

isInside <- function(point, square., square.point2, square.point3, square.point4) {
  
}