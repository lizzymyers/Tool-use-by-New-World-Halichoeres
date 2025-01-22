# Code of the paper 'Tool use in New World Halichoeres' 
# Author: Juliette Tariel-Adam
# Date: 11 July 2024
# This code maps the observations of fish anvil use on a world map

# Packages
library(ggplot2)
library(ggrepel)
library(maps)
library(tidyverse)

# Read data
CoordLit <- read.csv("Figure map/Table-literature-for-map.csv", header = TRUE)
CoordObs <- read.csv("Figure map/Table-observations-for-map.csv", header = TRUE)
Coord <- rbind(CoordLit, CoordObs)
str(Coord)
Distri <- read.csv("Figure map/Point-distribution_FishBase_June24.csv", header = TRUE)
str(Distri)

# Arrange data 
Coord <- Coord %>%  filter(complete.cases(.)) %>% # filter rows without NA\
  mutate(Approx_lat = round (Approx_lat, 1),
         Approx_long = round(Approx_long, 1),# round coordinates
         Species = sub("Halichoeres", "H.", Species)) %>%  # replace Halichoeres by H.
  distinct() # filter unique rows
Coord

Distri <- Distri %>% select(Center.Lat,Center.Long) %>% distinct()

world_map <- map_data("world")
# Colour of box segment
Coord$Color <-  ifelse(Coord$Type == "Literature", "blue", "red")
# Colour of box fill
Coord$Color2 <- ifelse(Coord$Type == "Literature", "#EEEBFF", "#FFEBEB")

# Remove space in the species name
Coord$Species <- sub("\\s+$", "", Coord$Species)
# italic species name for the figure
Coord$Species <- paste0("italic('",Coord$Species, "')")

map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "white", color = gray(0.5)) +
  geom_point(data = Distri, aes(x = Center.Long, y = Center.Lat), size = 1, color = "#69a7b8ff") +
  geom_point(data = Coord, aes(x = Approx_long, y = Approx_lat, color = Type), size = 2) +
  geom_label_repel(data = Coord, aes(x = Approx_long, y = Approx_lat, label = Species),
                   parse = TRUE, # for the italic
                   box.padding = 0.10, point.padding = 0.1, label.padding = 0.1,
                   size = 2, # size font text
                   segment.size = 0.5,
                   min.segment.length = 0,
                   max.time = 30,
                   max.iter = 100000,  
                   color = "black",
                   segment.color = Coord$Color,
                   fill = Coord$Color2) + 
  theme(panel.background = element_rect(fill = "lightblue"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.justification=c(1,1), legend.position=c(1,1),
        legend.background = element_rect(linetype = "solid", color = "black"),
        legend.text = element_text(size=5),
        legend.title = element_text(size=5)) +
  ylim(-60,90)+
  scale_color_manual(name = "Type of observation", values = c("blue","red")
)
map
# save the map as svg for later editing in Inkscape
ggsave(map, filename = "Figure map/Figure-map-output-R.svg",dpi = 320, width = 17 , height = 8, units = "cm") 