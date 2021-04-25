#'Calculate the continuity of street buildings
#'@description
#Calculates the spatial continuity of the buildings on two sides of street with two options available.
#'It generates a buffer area with distance di on each side of street. It then projects buildings within this area onto street lines. Continuity of buildings equals the ratio of projection length of buildings to the length of streets.
#'In option regular, the projection length of buildings is defined as the length of all building projection on each side of streets.
#'In option near, the projection length of buildings is defined as the length of projection of one side of building which has the shortest distance to streets.


#'@usage
#'building_continuity(building, street, di, option = ("regular", "near"))
#'
#'@param building 	A Simple Feature containing the building footprints of a city
#'@param street 	A Simple Feature containing the street network of a city.
#'@param di 	Defines the distance from streets. Buildings within which area will be considered as buildings alongside street (default = 40 meters).
#'@param w 	defines the total distance of the translation of the street segment (line) in option1/2 (default = 40 meters). Defines the length of perpendicular line in option3 (default = 100 meters).
#'@param option option 	defines the method by which continuity of street buildings is calculated.
#'If option = "regular", the projection length of buildings is defined as the length of all building projection on each side of streets.
#'If option = "near", the projection length of buildings is defined as the length of projection of the nearest building edge to the street segment.
#'@details
#'It returns the continuity (0 to 1) of the buildings alongside every street to the simple feature "street". If the translation lines of streets and the polygons ("city") does not have intersection, it will return NA.
#'@examples
#'## Not run:
#'## Not run:
#'building <-data(building)
#'street<-data(street)
#'continuity(building, street, di = 40, option = "regular")

#'building <-data(building)
#'street<-data(street)
#'continuity(building, street, di = 40, option = "near")
#'## End(Not run)


continuity <- function(city,road,di,option) {
  #di: buffer的宽度
  library(rgdal)
  library(raster)
  library(rgeos)
  library(sf)
  library(maptools)
  library(dplyr)
  library(geosphere)

  #数据类型转换

  city <- st_buffer(city,0)
  city$id<-1:nrow(city)

  #生成在建筑轮廓数据范围内的道路编号，节省对不与建筑轮廓相交的道路的计算时间
  bb = st_as_sfc(st_bbox(city))
  int <- st_intersects(bb,road) %>% unlist() %>% unique()

  #把建筑数据分为5km*5km的块，每个建筑对应一个part编号
  temp <- st_centroid(city)
  city$cx <-st_coordinates(temp)[,1]
  city$cy <-st_coordinates(temp)[,2]
  boxminx <- st_bbox(city)["xmin"]
  boxminy <- st_bbox(city)["ymin"]
  partx <- floor((st_bbox(city)["xmax"]-boxminx)/5000)
  party <- floor((st_bbox(city)["ymax"]-boxminy)/5000)
  city$px <- floor((city$cx-boxminx)/5000)+1
  city$py <- floor((city$cy-boxminy)/5000)+1
  city$part <- (city$py-1) * (partx+1) + city$px

  #建立result表格
  road_continuity <- character (nrow(road))

  #option1,每块建筑的投影
  if (option == "regular") {
    for (i in int) {
      print(i)
      road_i<-road[i,]

      #以道路中点所在的part确定和哪个part的建筑数据做intersection
      road_c <- st_centroid(road_i) %>% st_coordinates()
      rpartx <- floor((road_c[1]-boxminx)/5000)+1
      rparty <- floor((road_c[2]-boxminy)/5000)+1
      rpart <- (rparty-1) * (partx+1) + rpartx

      buffer <- st_buffer(road_i,di)
      buffer <- st_buffer(buffer,0)

      skip <- FALSE
      tryCatch(intersect <- st_intersection(buffer,filter(city,part==rpart)) ,
               error = function(e) { skip <<- TRUE})
      if(skip) { next }

      #intersect<-st_intersectionst_intersection(roadbuffer,filter(city,part==rpart))
      multipoint<-st_cast(intersect, "POINT")

      if (nrow(multipoint) == 0) {
        road_continuity[i] <- 0
        next
      } else {

        #得到点到路面的投影
        multipoint<-as(multipoint, 'Spatial')
        multipoint<-spTransform(multipoint, CRS("+init=epsg:3857"))
        buffer<-as(buffer,'Spatial')
        road_i <- as(road_i,'Spatial')
        project<-snapPointsToLines(multipoint, road_i,maxDist=40)

        #对投影长度加总（分别循环每一个建筑）
        q <- unique(project$id)
        sum = 0
        for (k in 1:length(q)) {
          temp_pj <- project[project$id == q[k],]
          max = max(spDists(temp_pj))
          sum = sum + max
        }

        road_continuity[i] <- sum/gLength(road_i)
      }
    }
  }

  #option2，每块建筑最近的边的投影
  if (option == "near") {
    for (i in int){
      road_i<-road[i,]
      buffer<-st_buffer(road_i,di)
      buffer <- st_buffer(buffer,0)

      #以道路中点所在的part确定和哪个part的建筑数据做intersection
      road_c <- st_centroid(road_i) %>% st_coordinates()
      rpartx <- floor((road_c[1]-boxminx)/5000)+1
      rparty <- floor((road_c[2]-boxminy)/5000)+1
      rpart <- (rparty-1) * (partx+1) + rpartx

      skip <- FALSE
      tryCatch(intersect <- st_intersection(buffer,filter(city,part==rpart)) ,
               error = function(e) { skip <<- TRUE})
      if(skip) { next }

      multipoint<-st_cast(intersect, "MULTIPOINT") %>% st_cast("POINT")
      if (nrow(multipoint) == 0) {next}

      multipoint$distance <- st_distance(multipoint,road_i)
      multipoint <- group_by(multipoint, id)
      point_near_1st <- filter(multipoint, rank(distance, desc(distance),ties.method="first")==1)
      point_near_1st <- point_near_1st [order(point_near_1st$id), ]
      point_near_2st <- filter(multipoint, rank(distance, desc(distance),ties.method="first")==2)
      point_near_2st <- point_near_2st [order(point_near_2st$id), ]

      # 计算投影并加总
      id_count = unique(point_near_1st$id)
      distance <- nrow(id_count)
      distance <- st_distance(point_near_1st,point_near_2st,by_element = TRUE)

      road_continuity[i] <- sum(distance)/st_length(road_i)
      }
    }

  road$continuity <- road_continuity
}
