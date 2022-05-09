library(osmdata)
library(sf)
library(tidyverse)

# Set the boundaries of the map here. This is Oxford: 
min_lon <- 51.71; max_lon <- 51.79
min_lat <- -1.291371; max_lat <- -1.192764

# Make a bbx object with the boundaries
bbx <- rbind(x=c(min_lat, max_lat),
             y=c(min_lon, max_lon))
colnames(bbx) <- c("min","max")

# Extract selected features 
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
                  value = c("river", "riverbank", "canal", "stream")) %>%
  osmdata_sf()

water <- bbx %>%
  opq()%>%
  add_osm_feature(key = "water") %>%
  osmdata_sf()

rail <- bbx %>%
  opq() %>%
  add_osm_feature(key = "railway", 
                  value = c("rail")) %>%
  osmdata_sf()

# Set whatever this is to 4326 because otherwise it won't work
st_crs(streets$osm_lines) <- 4326
st_crs(highways$osm_lines) <- 4326
st_crs(rivers$osm_lines) <- 4326
st_crs(water$osm_polygons) <- 4326
st_crs(water$osm_multipolygons) <- 4326
st_crs(paths$osm_lines) <- 4326
st_crs(rail$osm_lines) <- 4326

# And plot! Every feature has a separate graph, so line width ("size") 
# and transparency ("alpha") can be modified independently. 
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
          size = .15)+
  geom_sf(data = water$osm_multipolygons, 
          col = NA,
          fill = "red",
          size = 0.1,
          alpha = 1) +
  geom_sf(data = water$osm_polygons, 
          col = "black",
          fill = "white",
          size = 0.1,
          alpha = 1) +
  geom_sf(data = water$osm_multipolygons, 
          col = "black",
          fill = "white",
          size = 0.1,
          alpha = 1) +
  geom_sf(data = paths$osm_lines,
          col = "black",
          size = .1,
          linetype = 3) +
  geom_sf(data = rail$osm_lines,
          col = "black",
          size = .15,
          linetype = 1342) + 

  coord_sf(xlim = c(min_lat, max_lat),
           ylim = c(min_lon, max_lon),
           expand = FALSE) +
  theme(legend.position = F) + theme_void()

#ggsave('oxford_map.pdf')





