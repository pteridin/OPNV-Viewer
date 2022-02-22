library(sf)
library(leaflet)
library(tidyverse)
library(units)

circles_draw <- function(df, dist=100) {
  st_as_sf(df, 
           coords = c("lon", "lat"), crs = 4326) %>% 
    st_transform(crs = 32632) %>%  
    st_buffer(dist = set_units(dist, m)) %>% 
    sf::st_transform('+proj=longlat +datum=WGS84') %>% 
    st_union()
}

circles_multiple <- function(df,
                             dist = c(500, 1000, 2000, 5000, 10000)) {
  lapply(dist, 
         function(d) {
           circles <- circles_draw(df, d) %>% 
             st_sf(df %>% 
                     mutate(distance = d), geom=.)
         }) %>% 
    do.call(rbind, .) %>% 
    st_difference()
} 

set.seed(123)
sampleLocations <- data.frame(lat = c(51.519288),
                              lon = c(6.573943))

sampleLocations <- tibble(
  lat = c(sampleLocations$lat, sampleLocations$lat + runif(12, 0, .05)),
  lon = c(sampleLocations$lon, sampleLocations$lon + runif(12, 0, .05))
)


multiCircles <- circles_multiple(sampleLocations)


## Leaflet -------------
binpal <- colorQuantile("YlOrRd", domain = multiCircles$distance, 5)

sampleLocations %>% 
  head(1) %>% 
  leaflet() %>% 
    addTiles() %>% 
    addPolygons(data = multiCircles, 
                color = ~binpal(distance),
                fillColor = ~binpal(distance), 
                fillOpacity = .5,
                weight = 1) %>% 
    addMarkers()


