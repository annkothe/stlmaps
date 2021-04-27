#R OMS Project
#Angela Kothe
#04.25.2021

library(remotes)
library(sf)
remotes::install_github("ropensci/osmdata")
library(tidyverse)
library(osmdata)
library(showtext)
library(ggmap)

#data
stlouis<-getbb("St. Louis Missouri")

stlroads <- stlouis%>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary", "motorway_link", "trunk_link", "primary_link")) %>%
  osmdata_sf()

stlstreets2 <- stlouis%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


stlstreets3 <- stlouis%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

stlriver <- stlouis%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

stlcoast <- stlouis%>%
  opq()%>%
  add_osm_feature(key = "natural", value="coastline") %>%
  osmdata_sf()

stlwaters <- stlouis%>%
  opq()%>%
  add_osm_feature(key = "natural", value="water") %>%
  osmdata_sf()

stltrain <- stlouis%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

#base map
font_add_google(name = "Lato", family = "lato") 
showtext_auto()
ggplot() +
  geom_sf(data = stlcoast$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = stlcoast$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = stltrain$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = stlstreets2$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = stlstreets3$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = stlroads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = stlouis[1,], 
           ylim = stlouis[2,],
           expand = FALSE) +
  theme_void() + 
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "ST. LOUIS", subtitle = "38.6270° N/ 90.1994° W")
ggsave(file="stl.pdf", units="in", width=6, height=7)


#Bars in St. Louis
bars <- stlouis%>%
  opq()%>%
  add_osm_feature(key = "amenity", value=c("bar")) %>%
  osmdata_sf()

bars

ggplot() +
  geom_sf(data = stlcoast$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = stlcoast$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = stltrain$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = stlstreets2$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = stlstreets3$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = stlroads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  geom_sf(data = bars$osm_points,
          inherit.aes = FALSE,
          color = "red4",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = stlouis[1,], 
           ylim = stlouis[2,],
           expand = FALSE) +
  theme_void() + 
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "Bars", subtitle = "St. Louis")
ggsave(file="stlbars.pdf", units="in", width=6, height=7)

#Map Including Anheuser-Bush (breweries)
breweries <- stlouis%>%
  opq()%>%
  add_osm_feature(key = "craft", value=c("brewery")) %>%
  osmdata_sf()

breweries

ggplot() +
  geom_sf(data = stlcoast$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .3,
          fill = "white") +
  geom_sf(data = stlcoast$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .3) +
  geom_sf(data = stlwaters$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .15) +
  geom_sf(data = stltrain$osm_lines,
          inherit.aes = FALSE,
          color = "red",
          size = .2,
          linetype="dotdash",
          alpha = .9) +
  geom_sf(data = stlstreets2$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = stlstreets3$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = stlroads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  geom_sf(data = bars$osm_points,
          inherit.aes = FALSE,
          color = "red4",
          size = .5,
          alpha = .6) +
  geom_sf(data = breweries$osm_polygons,
          inherit.aes = FALSE,
          color = "yellow",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = stlouis[1,], 
           ylim = stlouis[2,],
           expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "Map with Bars and Breweries", subtitle = "St. Louis")

#Places Angela has lived in St. Louis City
tholozan<-data.frame(
  longitude = c(-90.267260), 
  latitude = c(38.593170))

hollyhills<-data.frame(
  longitude = c(-90.255330), 
  latitude = c(38.570690))

soulard<-data.frame(
  longitude = c(-90.214290), 
  latitude = c(38.600620))

ggplot() +
  geom_sf(data = stlcoast$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .3,
          fill = "white") +
  geom_sf(data = stlcoast$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .3) +
  geom_sf(data = stlwaters$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .15) +
  geom_sf(data = stltrain$osm_lines,
          inherit.aes = FALSE,
          color = "red",
          size = .2,
          linetype="dotdash",
          alpha = .9) +
  geom_sf(data = stlstreets2$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = stlstreets3$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = stlroads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  geom_sf(data = bars$osm_points,
          inherit.aes = FALSE,
          color = "red4",
          size = .5,
          alpha = .6) +
  geom_sf(data = breweries$osm_polygons,
          inherit.aes = FALSE,
          color = "yellow",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = stlouis[1,], 
           ylim = stlouis[2,],
           expand = FALSE) +
  geom_point(data = hollyhills, aes(x =longitude, y=latitude), 
             shape = 19, color = "skyblue", size = .5, stroke = 1) +
  geom_point(data = tholozan, aes(x =longitude, y=latitude), 
             shape = 19, color = "skyblue", size = .5, stroke = 1) +
  geom_point(data = soulard, aes(x =longitude, y=latitude), 
             shape = 19, color = "skyblue", size = .5, stroke = 1) +
  theme_void() +
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "Places Angela Lived Summer 2020", subtitle = "in St. Louis")