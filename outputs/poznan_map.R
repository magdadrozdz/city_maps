library(osmdata)
library(sf)
library(tidyverse)

#more specific coordinates
min_lon <- 52.351147; max_lon <- 52.451320
min_lat <- 16.848281; max_lat <- 16.982051
bbx <- rbind(x=c(min_lat,max_lat),y=c(min_lon, max_lon))
colnames(bbx) <- c("min","max")


highways <- bbx %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()

streets <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service", "unclassified",
                            "track", "bridleway", "cycleway")) %>%
  osmdata_sf()

paths <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("pedestrian", "footway", "path")) %>%
  osmdata_sf()

rivers <- bbx %>%
  opq()%>%
  add_osm_feature(key = "waterway", 
                  value = c("river", "riverbank", "canal", "stream", "drain", "ditch")) %>%
  osmdata_sf()

landuse <- bbx %>%
  opq() %>%
  add_osm_feature(key = "landuse", 
                  value = c("recreation_ground")) %>%
  osmdata_sf()

rail <- bbx %>%
  opq() %>%
  add_osm_feature(key = "railway", 
                  value = c("rail")) %>%
  osmdata_sf()

buildings <- bbx %>%
  opq()%>%
  add_osm_feature(key = "building", 
                  value = c("house", "train_station", "school", "commercial", "cathedral", "religious","public", "university", "dormitory", "civic", "sports_hall")) %>%
  osmdata_sf()

st_crs(streets$osm_lines) <- 4326
st_crs(highways$osm_lines) <- 4326
st_crs(rivers$osm_lines) <- 4326
st_crs(cycleway$osm_lines) <- 4326
st_crs(paths$osm_lines) <- 4326
st_crs(buildings$osm_lines) <- 4326
st_crs(rail$osm_lines) <- 4326
st_crs(landuse$osm_lines) <- 4326

ggplot() +
  geom_sf(data = streets$osm_lines,
          col = "black",
          size = .15,
          alpha = 0.8)+
  geom_sf(data = highways$osm_lines,
          col = "black",
          size = .13,
          alpha = 0.9)+
  geom_sf(data = rivers$osm_lines,
          col = "black",
          size = .15,
          alpha = 1)+
  geom_sf(data = paths$osm_lines,
          col = "black",
          size = .1,
          linetype = 3,
          alpha = 1) + 
  geom_sf(data = buildings$osm_lines,
          col = "black",
          size = .15,
          linetype = 1,
          alpha = 1) + 
  geom_sf(data = rail$osm_lines,
          col = "black",
          size = .15,
          linetype = 1342,
          alpha = 1) + 
  geom_sf(data = landuse$osm_lines,
          col = "black",
          size = .15,
          linetype = 1,
          alpha = 1) + 
  coord_sf(xlim = c(min_lat, max_lat),
           ylim = c(min_lon, max_lon),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()

ggsave('poznan_map.pdf')

