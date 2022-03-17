timeToInt <- function(time) {
  hour(time)*3600+minute(time)*60+second(time)
}

hmsToInt <- function(h, m, s) {
  h*3600+m*60+s
}

makeCircle <- function(timeDistance = 10L) {
  nodeList_Points %>% 
    filter(offset < timeDistance) %>% 
    mutate(Radius = ((timeDistance - offset) %/% (STEP_SIZE) + 1)*STEP_DISTANCE) %>% 
    st_buffer(.,
              dist = units::set_units(.$Radius, m),
              nQuadSegs = 10) %>% 
    st_union() %>% 
    st_transform('+proj=longlat +datum=WGS84') %>% 
    st_sf(tibble(distance = timeDistance), 
          geom=.)
}