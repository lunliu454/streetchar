#'Calculate building volume
#'@description
#'Calculates the volume of buildings on two sides of streets with two options available.
#'It performs geometric translation to the lines ("street") by distance k, and defines polygons ("buildings") that intersect with the lines as building alongside streets.

#'@usage
#'Building_volume(building, street, option = ("fixed", "street"), k = )
#'
#'@param building 	A Simple Feature containing the building footprints of a city
#'@param street 	A Simple Feature containing the street network of a city.
#'@param option Defines the method by which building volume is calculated.
#'If option = "fixed", k should be defined by the user (default = 40 meters).
#'If option = "street", k equals to the width of street on each side.
#'@param k Defines the distance from streets. Buildings within which area will be considered as buildings alongside streets.
#'@details
#'Calculate building volume with two options available.
#'It returns the summary statistics (max, min, average, weighted average, standard deviation) of building area alongside every street to the Simple Feature "street". If the calculation is performed in option "street", the width of street should first be calculated
#'If the translation lines of streets and the polygons ("building") does not have intersection, it will return NA.
#' @examples
#'## Not run:
#'building <-data(building)
#'street<-data(street)
#'building_volume (building, street, option = "fixed", k = 40)

#'building <-data(building)
#'street<-data(street)
#'building_volume (building, street, option = "street", k = 40)
#'## End(Not run)
building_volume <- function(city,road,option,k){

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

  volume_mean <- character (nrow(road))
  volume_weighted.mean <- character (nrow(road))
  volume_median <- character (nrow(road))
  volume_min <- character (nrow(road))
  volume_max <- character (nrow(road))
  volume_sd <- character (nrow(road))

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
        volume_mean[i] <- NA
        volume_weighted.mean[i] <- NA
        volume_median[i] <- NA
        volume_min[i] <- NA
        volume_max[i] <- NA
        volume_sd[i] <- NA
      } else {
        inter_building <- filter(city2, city2$id %in% intersect$id)
        inter_building$area <- st_area(inter_building) %>% as.numeric
        inter_building$volume <- inter_building$area * inter_building$floors
        intersect$length <- st_length(intersect) %>% as.numeric

        #计算指标
        volume_mean[i] <- mean(inter_building$volume)
        volume_weighted.mean[i] <- weighted.mean(inter_building$volume,intersect$length)
        volume_median[i] <- median(inter_building$volume)
        volume_min[i] <- min(inter_building$volume)
        volume_max[i] <- max(inter_building$volume)
        volume_sd[i] <- sd(inter_building$volume)
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
          volume_mean[i] <- NA
          volume_weighted.mean[i] <- NA
          volume_median[i] <- NA
          volume_min[i] <- NA
          volume_max[i] <- NA
          volume_sd[i] <- NA
        } else {
          inter_building <- filter(city2, city2$id %in% intersect$id)
          inter_building$area <- st_area(inter_building) %>% as.numeric
          inter_building$volume <- inter_building$area * inter_building$floors
          intersect$length <- st_length(intersect) %>% as.numeric

          #计算指标
          volume_mean[i] <- mean(inter_building$volume)
          volume_weighted.mean[i] <- weighted.mean(inter_building$volume,intersect$length)
          volume_median[i] <- median(inter_building$volume)
          volume_min[i] <- min(inter_building$volume)
          volume_max[i] <- max(inter_building$volume)
          volume_sd[i] <- sd(inter_building$volume)
        }
      }
    }
      road$volume_mean <- volume_mean
      road$volume_mean <- volume_weighted.mean
      road$volume_median <- volume_median
      road$volume_min <- volume_min
      road$volume_max <- volume_max
      road$volume_sd <- volume_sd
}
