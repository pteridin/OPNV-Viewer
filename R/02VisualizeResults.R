MAX_TIME_DISTANCE <- 180
STEP_SIZE <- 30
STEP_DISTANCE <- STEP_SIZE*params$TICK_DISTANCE

nodeList_Points <- st_as_sf(traveltimes_Calc %>% 
                              select(lon, lat, offset = duration_Mean), 
                            coords = c("lon", "lat"), 
                            crs = 4326) %>% 
  st_transform(crs = 32632) 

# Make Circles ----------
nodeList_Circles <- lapply(seq(STEP_SIZE,MAX_TIME_DISTANCE, by=STEP_SIZE),
                           makeCircle) %>% 
  do.call(bind_rows, .) %>% 
  st_difference()


# Plot ----------  
dist_pal <- colorBin("magma",
                     bins = MAX_TIME_DISTANCE/STEP_SIZE,
                     domain = nodeList_Circles$distance,
                     reverse = T)

nodeList_Circles %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~dist_pal(distance),
              color = ~dist_pal(distance)) %>% 
  addLegend(pal = dist_pal, values = ~distance, opacity = 1) -> Map

Map

htmlwidgets::saveWidget(Map, 
                        glue::glue("Results/{params$NAME}.html"))

traveltimes %>% 
  filter(str_detect(stop_name,"Langenfeld Schnei"))


gtfs_route(from = "de:05111:18135:1:1",
           to = "de:05158:19640:0:2_G",
           gtfs = gtfs,
           start_time = hmsToInt(17,0,0),
           day = params$START_DAY,
           max_transfers  = 2,
           from_to_are_ids = T)
