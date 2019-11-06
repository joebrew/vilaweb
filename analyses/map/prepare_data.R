# Libraries
library(tidyverse)
library(lubridate)
library(rgdal)
library(ggplot2)
library(ggthemes)

# Map
unzip('data/NUTS_BN_01M_2016_3035_LEVL_1.shp.zip')
dir('data/NUTS_BN_01M_2016_3035_LEVL_1.shp.zip')

map <- readOGR('regions', 'NUTS_RG_01M_2016_3035_LEVL_2')
original_proj <- proj4string(map)
map <- map[map@data$CNTR_CODE != 'TR',]
map <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))

# Reproject as lat long

# Fortify
map <- fortify(map, region = 'FID')
map <- map %>% filter(long >= -13,
                      long <= 33,
                      lat >= 25)
map$col <- map$id == 'ES51'

ggplot(data = map) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group,
                   fill = col),
               color = 'black',
               lwd = 0.1) +
  theme_map() +
  scale_fill_manual(name = '',
                    values = c('lightblue', 'red')) +
  theme(legend.position = 'none') +
  labs(title = 'Regions of Europe in which a majority want a self-determination\nreferendum, but are not permitted to carry one out') +
  theme(plot.title = element_text(size = 23))

