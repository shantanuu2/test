ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}

GeoDistanceInMetresMatrix  = function(df.geopoints){
  # install.packages("Imap", "leafletR","magrittr")
  df = read.csv("Ch7_bike_kiosk_locations.csv")
  head(df)
  names(df) = c("lat", "lon")
  
  # distance matrix
  df.dist = GeoDistanceInMetresMatrix(df)
  
  library(leaflet)
  library(magrittr)
  library(htmltools)
  
  #### Creating the map with polylines
  map <- leaflet() %>% 
    addTiles() %>%
    addCircleMarkers(data=df, lat = ~lat, lng = ~lon, radius = 3, popup = ~as.character(paste0(lat,",",lon))) %>%
    addPolylines(data=df, lng = ~lon, lat = ~lat,group = "Line")
  #Adding layer control
  map <- addLayersControl(map,
                          overlayGroups = c("Line"),
                          options = layersControlOptions(collapsed = TRUE)
  )
  
  show(map)
  
  
  #### Add rectangle
  
  # get a box of nodes on the way
  a = c(38.944474,-77.056286)
  b = c(38.898837,-76.983047)
  
  min.lat = min(a[1],b[1])
  min.lon = min(a[2],b[2])
  
  max.lat = max(a[1],b[1])
  max.lon = max(a[2],b[2])
  
  # number of nodes in box
  route = df[df$lat < max.lat & df$lat > min.lat & df$lon < max.lon & df$lon > min.lon,]
  nrow(route)
  
  # show rectangle
  map <- leaflet() %>% 
    addTiles() %>%
    addCircleMarkers(data=route, lat = ~lat, lng = ~lon, radius = 3, popup = ~as.character(paste0(lat,",",lon))) %>%
    addRectangles(data=route, min.lon, min.lat, max.lon, max.lat) %>%
    addPolylines(data=route, lng = ~lon, lat = ~lat,group = "Line")
  #Adding layer control
  map <- addLayersControl(map,
                          overlayGroups = c("Line"),
                          options = layersControlOptions(collapsed = TRUE)
  )
  
  show(map)
  
  #### Traveling salesman problem
  
  # install.packages('gtools')
  # load library
  library(gtools)
  
  # create edges list
  ncities = nrow(df)
  kiosks = df[sample(nrow(df), ncities),]
  kiosks = rbind(kiosks,a,b) # add target and source nodes
  rownames(kiosks) = 1:nrow(kiosks)
  
  # install.packages("TSP")
  library("TSP")
  
  matrx = GeoDistanceInMetresMatrix(kiosks)
  tsp <- TSP(df.dist)
  tsp
  
  tour <- solve_TSP(tsp)
  shrtpath = df[cut_tour(tour,"1"),]
  
  # show shortest path
  map <- leaflet() %>% 
    addTiles() %>%
    addCircleMarkers(data=df, lat = ~lat, lng = ~lon, radius = 3, popup = ~as.character(paste0(lat,",",lon))) %>%
    addPolylines(data=shrtpath, lng = ~lon, lat = ~lat,group = "Shortest Path", color = "#A93E36", opacity = 1)
  #Adding layer control
  map <- addLayersControl(map,
                          overlayGroups = c("Line", "Shortest Path"),
                          options = layersControlOptions(collapsed = TRUE)
  )
  
  show(map)