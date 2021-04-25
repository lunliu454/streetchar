
#'Calculate building area
#'@description
#'Calculates the area of buildings on two sides of streets. It performs geometric translation to the lines ("street") by distance k, and defines polygons ("building") that intersect with the lines as building alongside streets.
#'@usage
#'street_width (building, street, k = ，w =  , option = c("max","near","point"))
#'
#'@param building 	A Simple Feature containing the building footprints of a city
#'@param street 	A Simple Feature containing the street network of a city.
#'@param option Defines the method by which building area is calculated.
#'If option = "fixed", k should be defined by the user (default = 40 meters).
#'If option = "street", k equals to the width of street on each side.
#'@param k Defines the distance from streets. Buildings within which area will be considered as buildings alongside streets.
#'@details
#'Calculate building area with two options available.
#'It returns the summary statistics (max, min, average, weighted average, standard deviation) of building area alongside every street to the Simple Feature "street". If the calculation is performed in option "street", the width of street should first be calculated
#'If the translation lines of streets and the polygons ("building") does not have intersection, it will return NA.
#' @examples
#'## Not run:
#'building <-data(building)
#'street<-data(street)
#'building_area (building, street, option = "fixed", k = 40)

#'building <-data(building)
#'street<-data(street)
#'building_area (building, street, option = "street", k = 40)
#'## End(Not run)

building_area <- function(city,road,option,k){

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

  area_mean <- c(nrow(road))
  area_weighted.mean <- c(nrow(road))
  area_median <- c(nrow(road))
  area_min <- c(nrow(road))
  area_max <- c(nrow(road))
  area_sd <- c(nrow(road))

  if (option == "street"){

    k_left <- road$widthLeft
    k_right <- road$widthRight
    for (i in (1:length(road))){
      road_i <- road2[i,]
      roadbuffer1 <- st_buffer(road_i,k_left[i],endCapStyle = "FLAT",singleSide=T)
      roadbuffer2 <- st_buffer(road_i,k_right[i],endCapStyle = "FLAT",singleSide=T)
      roadbuffer <- rbind(roadbuffer1,roadbuffer2)
      roadbuffer <- st_cast(roadbuffer,"LINESTRING")


      #和建筑交集并算长???
      intersect <- st_intersection(roadbuffer,city2)
      if (nrow(intersect) == 0) {
        num_mean[i] <- NA
        num_weighted.mean[i] <- NA
        num_median[i] <- NA
        num_min[i] <- NA
        num_max[i] <- NA
        num_sd[i] <- NA
      } else {
      inter_building <- filter(city2, city2$id %in% intersect$id)
      inter_building$area <- st_area(inter_building) %>% as.numeric
      intersect$length <- st_length(intersect) %>% as.numeric

      #计算指标
      area_mean[i] <- mean(inter_building$area)
      area_weighted.mean[i] <- weighted.mean(inter_building$area,intersect$length)
      area_median[i] <- median(inter_building$area)
      area_min[i] <- min(inter_building$area)
      area_max[i] <- max(inter_building$area)
      area_sd[i] <- sd(inter_building$area)
      }
    }
  }

  if (option == "fixed"){
    for (i in (1:length(road))){
      if (k == 0 | is.na(k)) {
        k = 40
      }
      road_i <- road2[i,]
      roadbuffer <- st_buffer(road_i,k)
      roadbuffer <- st_cast(roadbuffer,"LINESTRING")

      #和建筑交集并算长???
      intersect <- st_intersection(roadbuffer,city2)
      if (nrow(intersect) == 0) {
        num_mean[i] <- NA
        num_weighted.mean[i] <- NA
        num_median[i] <- NA
        num_min[i] <- NA
        num_max[i] <- NA
        num_sd[i] <- NA
      } else {
      inter_building <- filter(city2, city2$id %in% intersect$id)
      inter_building$area <- st_area(inter_building) %>% as.numeric
      intersect$length <- st_length(intersect) %>% as.numeric

      #计算指标
      area_mean[i] <- mean(inter_building$area)
      area_weighted.mean[i] <- weighted.mean(inter_building$area,intersect$length)
      area_median[i] <- median(inter_building$area)
      area_min[i] <- min(inter_building$area)
      area_max[i] <- max(inter_building$area)
      area_sd[i] <- sd(inter_building$area)
      }
    }
  }

  road$area_mean <- area_mean
  road$area_mean <- area_weighted.mean
  road$area_median <- area_median
  road$area_min <- area_min
  road$area_max <- area_max
  road$area_sd <- area_sd
}

