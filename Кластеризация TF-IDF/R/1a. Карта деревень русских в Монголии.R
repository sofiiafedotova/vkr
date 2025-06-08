library(leaflet)
library(sf)

geo_data <- st_read("~/Диплом/points.geojson")

leaflet(data = geo_data) %>% 
  addTiles() %>% 
  addMarkers(lng = ~lon,
             lat = ~lat,
             popup = ~name,
             clusterOptions = markerClusterOptions())