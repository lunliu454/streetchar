#'Calculate the width of street defined by buildings
#'@description
#'Calculates  the width of the street space defined by the buildings on the two sides of the street with three options available.
#'In option max and 2 near, it performs geometric translation to the spatial lines ("street") by distance w (by translating lines with distance k each time).
#'In option max, building-to-building width is defined as the distance between original street lines and translation lines of street which have the max intersection with buildings. In option near, building-to-building width is defined as the distance between translation lines and street when translation lines first intersect with buildings.
#'In option point, it generates sample points by distance k alongside street and make perpendicular lines with length w. Building-to-building width is defined as the average length of the perpendicular lines that intersects with each side of streets.
#'@usage
#'street_width (building, street, k = ，w =  , option = c("max","near","point"))
#'
#'@param building 	A Simple Feature containing the building footprints of a city
#'@param street 	A Simple Feature containing the street network of a city.
#'@param k 	Defines the distance of every translation of the street segments (line) in option1/2 (default = 5 meters). Defines the distance between sample points in option3 (default = 200 meters).
#'@param w 	defines the total distance of the translation of the street segment (line) in option1/2 (default = 40 meters). Defines the length of perpendicular line in option3 (default = 100 meters).
#'@param option defines the method by which building-to-building width is calculated.
#'If option = "max", building-to-building width is calculated as the distance between translation lines of street which have the max intersection with buildings on each side and the original street line.
#'If option = "near", building-to-building width is calculated as the minimal distance between street and buildings on each side of streets.
#'If option = "point", building-to-building width is calculated as the average length of the perpendicular lines that intersects with each side of street.
#'@details
#'It returns the distance from the street to the buildings on its left and right sides to the simple feature "street". If the translation lines ("street") and the polygons ("building") does not have intersection, it will return NA.
#'@examples
#'## Not run:
#'building <-data(building)
#'street<-data(street)
#'street_width(building, street, k = 10, w = 50, option = "near")

street_width <- function(city,road,k,w,option){
  library(rgdal)
  library(devtools)
  library(sf)
  library(dplyr)
  library(maptools)
  library(stplanr)
  library(geosphere)

  city$id<-1:nrow(city)
  city <- st_transform(city,3857)
  city <- st_buffer(city,0)
  road <- st_transform(road,3857)

  #input
  j = ceiling(w/k)
  # j = max times of move

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

  if (option == "max") {
    #roadwidth 1
    #for loop
    roadwidthRight<-c()
    roadwidthLeft<-c()
    for (i in int) {
      print(i)
      road_i <- road[i,]
      #width <- 0 # 初始化路???

      # 向右移动道路
      #interlen <- data.frame(m=0,len=0)
      len_buffer <- seq(k,w,k)

      for (m in 1:length(len_buffer)) {
        roadbuffer <- st_buffer(road_i,len_buffer[m],endCapStyle = "FLAT",singleSide=T) %>% st_cast("MULTILINESTRING") %>% st_cast("LINESTRING")
        roadbuffer$roadid <- m
        assign(paste("roadbuffer_",m,sep=""),roadbuffer)
      }
      buffer <- lapply(ls(pattern="^roadbuffer_"), function(x) get(x))
      roadbuffer <- bind_rows(buffer)
      #roadbuffer <- rbind(tmp_buffer,roadbuffer)

      #以道路中点所在的part确定和哪个part的建筑数据做intersection
      road_c <- st_centroid(road_i) %>% st_coordinates()
      rpartx <- floor((road_c[1]-boxminx)/5000)+1
      rparty <- floor((road_c[2]-boxminy)/5000)+1
      rpart <- (rparty-1) * (partx+1) + rpartx

      skip <- FALSE
      tryCatch(intersect <- st_intersection(roadbuffer,filter(city,part==rpart)),
               error = function(e) { skip <<- TRUE})
      if(skip) { next }

      if (nrow(intersect) == 0) {
        #roadwidthRight[i] <- NA
        next}

      #找出intersect长度和最大的roadid
      aggregate_intersect <- intersect %>%
        group_by(roadid) %>%
        summarise()
      roadwidthRight[i] <- max(st_length(aggregate_intersect))

      # 向左移动道路
      len_buffer <- seq(k,w,k)

      for (m in 1:length(len_buffer)) {
        roadbuffer <- st_buffer(road_i,-len_buffer[m],endCapStyle = "FLAT",singleSide=T) %>% st_cast("MULTILINESTRING") %>% st_cast("LINESTRING")
        roadbuffer$roadid <- m
        assign(paste("roadbuffer_",m,sep=""),roadbuffer)
      }
      buffer <- lapply(ls(pattern="^roadbuffer_"), function(x) get(x))
      roadbuffer <- bind_rows(buffer)

      skip <- FALSE
      tryCatch(intersect <- st_intersection(roadbuffer,filter(city,part==rpart)),
               error = function(e) { skip <<- TRUE})
      if(skip) { next }

      if (nrow(intersect) == 0) {
        #roadwidthRight[i] <- NA
        next}

      #找出intersect长度和最大的roadid
      aggregate_intersect <- intersect %>%
        group_by(roadid) %>%
        summarise(.groups = 'drop')
      roadwidthRight[i] <- max(st_length(aggregate_intersect))
    }
    road$widthLeft <- roadwidthLeft
    road$widthRight <- roadwidthRight
  }
  if (option == "near") {
    roadwidthRight<-c()
    roadwidthLeft<-c()
    for (i in int) {
      print(i)
      road_i <- road[i,]
      #width <- 0 # 初始化路???

      # 向右移动道路
      #interlen <- data.frame(m=0,len=0)
      len_buffer <- seq(k,w,k)

      for (m in 1:length(len_buffer)) {
        roadbuffer <- st_buffer(road_i,len_buffer[m],endCapStyle = "FLAT",singleSide=T) %>% st_cast("MULTILINESTRING") %>% st_cast("LINESTRING")
        roadbuffer$roadid <- m
        assign(paste("roadbuffer_",m,sep=""),roadbuffer)
      }
      buffer <- lapply(ls(pattern="^roadbuffer_"), function(x) get(x))
      roadbuffer <- bind_rows(buffer)
      #roadbuffer <- rbind(tmp_buffer,roadbuffer)

      #以道路中点所在的part确定和哪个part的建筑数据做intersection
      road_c <- st_centroid(road_i) %>% st_coordinates()
      rpartx <- floor((road_c[1]-boxminx)/5000)+1
      rparty <- floor((road_c[2]-boxminy)/5000)+1
      rpart <- (rparty-1) * (partx+1) + rpartx

      skip <- FALSE
      tryCatch(intersect <- st_intersection(roadbuffer,filter(city,part==rpart)),
               error = function(e) { skip <<- TRUE})
      if(skip) { next }

      if (nrow(intersect) == 0) {
        #roadwidthRight[i] <- NA
        next}

      #找出intersect里面roadid最小的，就是第一次intersect不为0的
      roadwidthRight[i] <- sum(st_length(filter(intersect,roadid == min(intersect$roadid, na.rm = TRUE))))


      # 向左移动道
      len_buffer <- seq(k,w,k)

      for (m in 1:length(len_buffer)) {
        roadbuffer <- st_buffer(road_i,-len_buffer[m],endCapStyle = "FLAT",singleSide=T) %>% st_cast("MULTILINESTRING") %>% st_cast("LINESTRING")
        roadbuffer$roadid <- m
        assign(paste("roadbuffer_",m,sep=""),roadbuffer)
      }
      buffer <- lapply(ls(pattern="^roadbuffer_"), function(x) get(x))
      roadbuffer <- bind_rows(buffer)

      skip <- FALSE
      tryCatch(intersect <- st_intersection(roadbuffer,filter(city,part==rpart)),
               error = function(e) { skip <<- TRUE})
      if(skip) { next }

      if (nrow(intersect) == 0) {
        #roadwidthRight[i] <- NA
        next}

      #找出intersect里面roadid最小的，就是第一次intersect不为0的
      roadwidthLeft[i] <- sum(st_length(filter(intersect,roadid == min(intersect$roadid, na.rm = TRUE))))
    }
    road$widthLeft <- roadwidthLeft
    road$widthRight <- roadwidthRight
  }
  if (option == "point") {
    roadwidth <- c()
    for (i in int){
      print(i)

      road_i <- road[i,]
      numOfPoints <- (st_length(road_i) / k) %>% as.numeric()
      sample <- st_sample(road_i, size = ceiling(numOfPoints), type = "regular")
      ###识别取样点落入的道路段
      sample <- sample[[1]] %>%
        st_coordinates() %>%
        as.data.frame()
      sec <- road_i$geometry %>%
        st_coordinates() %>%
        as.data.frame() %>%
        distinct()
      sec$L1 <- mutate(sec,L1=(lead(X) - X)/(lead(Y) - Y))
      findsec <- function(a,b){
        j <- 1
        while (j < nrow(sec)){
          if (((a - sec[j,"X"]) * (a - sec[j+1,"X"]) <= 0) &
              ((b - sec[j,"Y"]) * (b - sec[j+1,"Y"]) <= 0)){
            return(sec[j,"L1"]$L1)
            break
          }
          j <- j + 1
        }
      }
      sample$angle <- mapply(findsec,sample$X,sample$Y)
      dx <- w / (unlist(sample$angle)^2 +1)
      dy <- w * unlist(sample$angle)^2 / (unlist(sample$angle)^2 +1)
      dy[is.na(dy)] <- w

      x1 <- sample[,1] + dx
      y1 <- sample[,2] + dy
      x2 <- sample[,1] - dx
      y2 <- sample[,2] - dy

      l<-lapply(1:nrow(sample),function(i)rbind(c(x1[i],y1[i]),c(x2[i],y2[i]))) #system.time 0.006
      L2 <- st_multilinestring(l) %>%
        st_sfc(crs=3857) %>%
        st_cast("LINESTRING")

      L2 <- st_sf(L2,lineid=1:length(L2))
      L2$px <- sample[,1]

      road_c <- st_centroid(road_i) %>% st_coordinates()
      rpartx <- floor((road_c[1]-boxminx)/5000)+1
      rparty <- floor((road_c[2]-boxminy)/5000)+1
      rpart <- (rparty-1) * (partx+1) + rpartx

      skip <- FALSE
      tryCatch(intersect <- st_intersection(L2,filter(city,part==rpart)) %>% st_cast('POINT'),
               error = function(e) { skip <<- TRUE})
      if(skip) { next }


      intersect$x <- st_coordinates(intersect)[,1]
      intersect$y <- st_coordinates(intersect)[,2]
      #st_geometry(intersect) <- NULL
      intersect <- intersect[order(intersect$lineid,intersect$x),]
      intersect <- intersect %>%
        group_by(lineid)%>%
        mutate(choose=(x-px)*(dplyr::lead(x)-px)<0)
      intersect <- intersect %>%
        mutate(road_wid=ifelse(choose==T,sqrt((lead(x)-x)^2+(lead(y)-y)^2),NA))
      roadwidth[i] <- mean(intersect$road_wid,na.rm = T)
    }
    road$width <- roadwidth
  }
}
