library(raster)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(sp)
library(rgdal)

# Get spatial data
borders <- rgdal::readOGR('data', 'ne_50m_admin_0_countries')

# Define functioning for jittering on water
jitter_water <- function(p){
  original_p <- p
  ok <- FALSE
  jitter_counter <- 0
  while(!ok){
    jitter_counter <- jitter_counter + 1
    # Jitter
    p <- p %>% mutate(x = x + rnorm(n = 1, mean = 0, sd = 0.08),
                      y = y + rnorm(n = 1, mean = 0, sd = 0.08))
    coordinates(p) <- ~x+y
    proj4string(p) <- proj4string(borders)
    # See if in land
    o <- over(p, polygons(borders))
    if(is.na(o) | jitter_counter > 10){
      ok <- TRUE
    } else {
      p <- original_p
    }
  }
  cp <- coordinates(p)
  out <- tibble(x = cp[1],
                y = cp[2])
  return(out)
}

if('df.RData' %in% dir()){
  load('df.RData')
} else {
  df <- read_csv('data/MissingMigrants-Global-2019-08-22T11-07-12.csv')
  # Keep just the med
  df <- df %>% filter(`Region of Incident` == 'Mediterranean')
  df$latitude <- df$y <- as.numeric(unlist(lapply(strsplit(df$`Location Coordinates`, ', '), function(x){x[1]})))
  df$longitude <- df$x <- as.numeric(unlist(lapply(strsplit(df$`Location Coordinates`, ', '), function(x){x[2]})))
  df <- df %>% 
    mutate(children = `Number of Children`) %>%
    mutate(children = ifelse(is.na(children), 0, children)) %>%
    mutate(`Total Dead and Missing` = ifelse(is.na(`Total Dead and Missing`), 0, `Total Dead and Missing`)) %>%
    mutate(adults = `Total Dead and Missing` - children) %>%
    mutate(date = as.Date(`Reported Date`, format = '%B %d, %Y')) %>%
    mutate(cause = `Cause of Death`)
  # Expand into a one row per person df
  new_list <- list()
  counter <- 0
  for(i in 1:nrow(df)){
    message(i)
    this_row <- df[i,]
    new_df <- this_row %>% dplyr::select(x,y,
                                         date,
                                         adults, 
                                         children,
                                         cause) 
    if(new_df$adults >= 1){
      for(j in 1:new_df$adults){
        new_row <- new_df %>% dplyr::select(x,y,date, cause) %>%
          mutate(type = 'adult') 
        counter <- counter + 1
        new_list[[counter]] <- new_row
      }
    }
    if(new_df$children >= 1){
      for(j in 1:new_df$children){
        new_row <- new_df %>% dplyr::select(x,y,date, cause) %>%
          mutate(type = 'nen') 
        counter <- counter + 1
        new_list[[counter]] <- new_row
      }
    }
  }
  
  df <- bind_rows(new_list)
  
  # Create a dictionary of death causes
  # df %>% group_by(cause) %>% tally %>% arrange(desc(n)) %>% write_csv('~/Desktop/dict.csv')
  
  # Read in the manual dict
  dict <- read_csv('dict.csv') %>%
    mutate(causa = as.character(causa)) %>%
    mutate(cause = as.character(cause)) %>%
    mutate(causa = ifelse(is.na(causa), cause, causa))
  df <- df %>% arrange(desc(type)) %>%
    left_join(dict, by = 'cause')
  
  # jitter the coordinates (while prioritizing keeping them on water)
  new_coords <- list()
  for(i in 1:nrow(df)){
    message(i)
    this_row <- df[i,]
    new_coords[[i]] <- jitter_water(p = this_row)
  }
  new_coords <- bind_rows(new_coords)
  
  df$xx <- new_coords$x
  df$yy <- new_coords$y
  save(df, file = 'df.RData')
}

df_sp <- df
coordinates(df_sp) <- ~xx+yy

popups <- paste0(df$date, ' | ',
                 'Un ', df$type, ' | Causa: ',
                 df$causa)

qpal <- colorFactor('RdYlBu', domain = sort(unique(df$type)))

l <- leaflet() %>%
  # addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addProviderTiles(providers$Stamen) %>%
  # addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012) %>%
  # addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircleMarkers(data = df_sp,
                   radius = 4,
                   fillOpacity = 0.2,
                   stroke = F,
                   color = qpal(df$type),
                   label = popups) %>%
  addLegend('bottomright',
            pal = qpal,
            values = sort(unique(df$type)),
            opacity = 1)

library(htmlwidgets)
saveWidget(l, file="map.html")
