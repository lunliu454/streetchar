#'Calculate building num
#'@description
#'Calculates the number of buildings on two sides of streets with two options available.
#'It performs geometric translation to the lines ("street") by distance k, and defines polygons ("buildings") that intersect with the lines as building alongside streets.

#'@usage
#'Building_num(building, street, option = ("fixed","street"), k = )
#'
#'@param building 	A Simple Feature containing the building footprints of a city
#'@param street 	A Simple Feature containing the street network of a city.
#'@param option Defines the method by which building num is calculated.
#'If option = "fixed", k should be defined by the user (default = 40 meters).
#'If option = "street", k equals to the width of street on each side.
#'@param k Defines the distance from streets. Buildings within which area will be considered as buildings alongside streets.
#'@details
#'Calculate building num with two options available.
#'It returns the summary statistics (max, min, average, weighted average, standard deviation) of building area alongside every street to the Simple Feature "street". If the calculation is performed in option "street", the width of street should first be calculated
#'If the translation lines of streets and the polygons ("building") does not have intersection, it will return NA.
#' @examples
#'## Not run:
#'building <-data(building)
#'street<-data(street)
#'building_num (building, street, option = "fixed", k = 40)

#'building <-data(building)
#'street<-data(street)
#'building_num (building, street, option = "street", k = 40)
#'## End(Not run)
building_num <- function(city,road,option,k){

  library(rgdal)
  library(devtools)
  library(sf)
  library(dplyr)
  library(maptools)
  library(stplanr)
  library(geosphere)
  library(rgeos)

  #格式转换
  road2<- st_as_sf(road)
  city2 <- st_as_sf(city)
  city2$id<-1:nrow(city2)
  city2 <- st_transform(city2,3857)
  city2 <- st_buffer(city2,0)
  road2<- st_transform(road2,3857)

  num_mean <- character (nrow(road))
  num_weighted.mean <- character (nrow(road))
  num_median <- character (nrow(road))
  num_min <- character (nrow(road))
  num_max <- character (nrow(road))
  num_sd <- character (nrow(road))

  if (option == "street"){

    k_left <- road$widthLeft
    k_right <- road$widthRight

    for (i in (1:length(road))){
      road_i <- road2[i,]
      roadbuffer1 <- st_buffer(road_i,k_left[i],endCapStyle = "FLAT",singleSide=T)
      roadbuffer2 <- st_buffer(road_i,k_right[i],endCapStyle = "FLAT",singleSide=T)
      roadbuffer <- rbind(roadbuffer1,roadbuffer2)
      roadbuffer <- st_cast(roadbuffer,"LINESTRING")

      intersect <- st_intersection(roadbuffer,city2)
      intersect$length <- st_length(intersect) %>% as.numeric
      if (nrow(intersect) == 0) {
        num_mean[i] <- NA
        num_weighted.mean[i] <- NA
        num_median[i] <- NA
        num_min[i] <- NA
        num_max[i] <- NA
        num_sd[i] <- NA
      } else {
        intersect$num <- length(unique(intersect$id))
        #计算指标
        num_mean[i] <- mean(intersect$num)
        num_weighted.mean[i] <- weighted.mean(intersect$num,intersect$length)
        num_median[i] <- median(intersect$num)
        num_min[i] <- min(intersect$num)
        num_max[i] <- max(intersect$num)
        num_sd[i] <- sd(intersect$num)
      }
    }
  }
  if (option == "fixed"){
    if (k == 0 | is.na(k)) {
      k = 40
    }
    for (i in (1:length(road))){
      road_i <- road2[i,]
      roadbuffer <- st_buffer(road_i,k)
      roadbuffer <- st_cast(roadbuffer,"LINESTRING")

      intersect <- st_intersection(roadbuffer,city2)
      intersect$length <- st_length(intersect) %>% as.numeric
      if (nrow(intersect) == 0) {
        num_mean[i] <- NA
        num_weighted.mean[i] <- NA
        num_median[i] <- NA
        num_min[i] <- NA
        num_max[i] <- NA
        num_sd[i] <- NA
      } else {
        intersect$num <- length(unique(intersect$id))
        #计算指标
        num_mean[i] <- mean(intersect$num)
        num_weighted.mean[i] <- weighted.mean(intersect$num,intersect$length)
        num_median[i] <- median(intersect$num)
        num_min[i] <- min(intersect$num)
        num_max[i] <- max(intersect$num)
        num_sd[i] <- sd(intersect$num)
      }
    }
    road$num_mean <- num_mean
    road$num_mean <- num_weighted.mean
    road$num_median <- num_median
    road$num_min <- num_min
    road$num_max <- num_max
    road$num_sd <- num_sd
 }
}
