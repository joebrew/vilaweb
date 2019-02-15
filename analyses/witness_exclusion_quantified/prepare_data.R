library(vilaweb)
library(rtweet)
library(tidyverse)
library(databrew)
library(gsheet)
# Read in google sheet with keys
if(!'goog.RData' %in% dir()){
  goog <- gsheet::gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1GhRC2nX3ysy2zPbwBqeVsah5gz-y2R6GFwxy2Ejt4lo/edit?usp=sharing')
  save(goog,
       file = 'goog.RData')
} else {
  load('goog.RData')
}

parties <- sort(unique(unlist(strsplit(paste0(goog$proposed_by, collapse = ' '), ' '))))

# Come up with a color matcher
color_matcher <-
  tibble(party = parties,
         defense = ifelse(party %in% c('Abogacia',
                                       'Fiscalia',
                                       'Vox'),
                          'Prosecution',
                          'Defense'))

# Assign an id number
goog$id <- 1:nrow(goog)

# Expand into long format
out_list <- list()
for(i in 1:length(parties)){
  this_party <- parties[i]
  sub_data <- goog %>%
    filter(grepl(this_party, proposed_by)) %>%
    mutate(proposed_by = this_party)
  out_list[[i]] <- sub_data
}
df <- bind_rows(out_list)

# Join the color_matcher
df <- left_join(df, color_matcher,
                by = c('proposed_by' = 'party'))
df$accepted <- df$status == 'Aceptado'

# Define translation function
translate <- function(x, to_ca = TRUE){
  translator <- 
    tibble(ca = c('Acceptat',
                  'Rebutjat',
                  'Acusació',
                  'Defensa',
                  'Fiscalia',
                  'Abogacia'),
           en = c('Accepted',
                  'Rejected',
                  'Prosecution',
                  'Defense',
                  'Public prosecutor',
                  'Attorney General'))
  x <- tolower(x)
  if(to_ca == TRUE){
    if(x %in% tolower(translator$en)){
      out <- translator$ca[tolower(translator$en) == x]
    } else {
      out <- x
    }
  } else {
    if(x %in% tolower(translator$ca)){
      out <- translator$en[tolower(translator$ca) == x]
    } else {
      out <- x
    }
  }
}
translate <- Vectorize(translate)

#Color helpers
bp <- function(x,n){
  RColorBrewer::brewer.pal(n = 9,
                           name = x)[n]
}

overall_plot <- function(language = 'ca',
                         return_table = FALSE){
  cols <- 
    c(bp('Blues',5),
      bp('Oranges', 5))
  pd <- df %>%
    group_by(defense) %>%
    summarise(rejected = length(unique(id[!accepted])),
              accepted = length(unique(id[accepted])),
              proposed = length(unique(id))) %>%
    ungroup
  if(return_table){
    return(pd)
  }
  pd <- pd %>%
    mutate(r = rejected / proposed * 100,
           p = accepted / proposed * 100) %>%
    gather(key, value, rejected:p)
  
  plot_data <- pd %>% filter(key %in% c('accepted', 'rejected'))
  percentages <- pd %>% filter(key %in% c('p', 'r')) %>%
    mutate(word = ifelse(key == 'p', 'accepted', 'rejected'))
  
  if(language == 'ca'){
    plot_data <- plot_data %>%
      mutate(key = translate(key),
             defense = translate(defense))
    percentages <- percentages %>%
      mutate(key = translate(key),
             defense = translate(defense),
             word = translate(word))
    x <- 'Testimonis'
    title <- 'Acceptació/Rebuig de testimonis'
    subtitle <- 'Acusació vs. Defensa | Judici del procés'
    caption <- 'Joe Brew | @joethebrew | www.vilaweb.cat'
  } else {
    x <- 'Witnesses'
    title <- 'Witness acceptance and rejection'
    subtitle <- 'Prosecution vs. Defense | Trial of Catalan independence leaders'
    caption <- 'Joe Brew | @joethebrew | www.vilaweb.cat'
  }
  percentages <- percentages %>%
    group_by(defense) %>%
    mutate(label = 
                paste0(round(value, digits = 2), ' % ', word)) %>%
    summarise(label = paste0(label, collapse = '\n'))

  # plot_data$key <- factor(plot_data$key,
  #                         levels = rev(sort(unique(plot_data$key))))
  plot_data$key <- Hmisc::capitalize(plot_data$key)
  ggplot(data = plot_data,
         aes(x = defense,
             y = value)) +
    geom_bar(stat = 'identity',
             aes(fill = key)) +
    coord_flip() +
    theme_vilaweb()  +
    geom_text(data = percentages,
              aes(label = label,
                  y = 60),
              size = 4,
              alpha = 0.7,
              hjust = 0) +
    scale_fill_manual(name = '',
                      values = cols,
                      guide = guide_legend(reverse = TRUE)) +
    labs(x = '',
         y = x,
         title = title,
         subtitle = subtitle,
         caption = caption)
}

break_plot <- function(language = 'ca',
                       return_table = FALSE){
  cols <- 
    c(bp('Blues',5),
      bp('Oranges', 5))
  pd <- df %>%
    group_by(defense, proposed_by) %>%
    summarise(rejected = length(unique(id[!accepted])),
              accepted = length(unique(id[accepted])),
              proposed = length(unique(id))) %>%
    ungroup
  if(return_table){
    return(pd)
  }
  pd <- pd %>%
    mutate(r = rejected / proposed * 100,
           p = accepted / proposed * 100) %>%
    gather(key, value, rejected:p)
  
  plot_data <- pd %>% filter(key %in% c('r'))
  percentages <- pd %>% filter(key %in% c('accepted', 'rejected')) %>%
    mutate(word = key)
  
  plot_data$proposed_by <- translate(plot_data$proposed_by, to_ca = F)
  
  if(language == 'ca'){
    plot_data <- plot_data %>%
      mutate(key = translate(key),
             defense = translate(defense),
             proposed_by = translate(proposed_by))
    percentages <- percentages %>%
      mutate(key = translate(key))
    x <- '% de testimonis rebutjats'
    title <- 'Rebuig de testimonis'
    subtitle <- 'Judici del procés'
    caption <- 'Joe Brew | @joethebrew | www.vilaweb.cat'
  } else {
    plot_data$defense <-
      translate(plot_data$defense,to_ca = FALSE)
    x <- '% of witnesses rejected by the court'
    title <- 'Witness rejection'
    subtitle <- 'Trial of Catalan independence leaders'
    caption <- 'Joe Brew | @joethebrew | www.vilaweb.cat'
  }
  percentages <- percentages %>%
    dplyr::filter(word %in% c('rejected', 'rebutjat')) %>%
    mutate(label = paste0(round(value, digits = 2),
                          '% ', word)) %>%
    mutate(y = value)
  
  plot_data$key <- Hmisc::capitalize(plot_data$key)
  plot_data$defense <- Hmisc::capitalize(plot_data$defense)
  plot_data$proposed_by <- Hmisc::capitalize(plot_data$proposed_by)
  plot_data <- plot_data %>%
    arrange(value)
  
  plot_data$proposed_by <- factor(plot_data$proposed_by,
                                  levels = plot_data$proposed_by)
  
  # plot_data$defense <- factor(plot_data$defense,
  #                             levels = rev(sort(unique(plot_data$defense))))
  
  # plot_data$key <- factor(plot_data$key,
  #                         levels = rev(sort(unique(plot_data$key))))
  ggplot(data = plot_data,
         aes(x = proposed_by,
             y = value,
             color = defense)) +
    geom_point(size = 3) +
    geom_segment(aes(yend = 0,
                     xend = proposed_by),
                 size = 2) +
    coord_flip() +
    theme_vilaweb()  +
    geom_text(data = plot_data,
              aes(label = paste0(round(value, digits = 2), '%'),
                  y = value + 3),
              size = 4,
              alpha = 0.7) +
    scale_color_manual(name = '',
                      values = cols,
                      guide = guide_legend(reverse = TRUE)) +
    labs(x = '',
         y = x,
         title = title,
         subtitle = subtitle,
         caption = caption)
}

police_plot <- function(language = 'ca',
                         return_table = FALSE){
  cols <- 
    c(bp('Blues',5),
      bp('Oranges', 5))
  pd <- df %>%
    filter(police) %>%
    group_by(defense) %>%
    summarise(rejected = length(unique(id[!accepted])),
              accepted = length(unique(id[accepted])),
              proposed = length(unique(id))) %>%
    ungroup
  if(return_table){
    return(pd)
  }
  pd <- pd %>%
    mutate(r = rejected / proposed * 100,
           p = accepted / proposed * 100) %>%
    gather(key, value, rejected:p)
  
  plot_data <- pd %>% filter(key %in% c('accepted', 'rejected'))
  percentages <- pd %>% filter(key %in% c('p', 'r')) %>%
    mutate(word = ifelse(key == 'p', 'accepted', 'rejected'))
  
  if(language == 'ca'){
    plot_data <- plot_data %>%
      mutate(key = translate(key),
             defense = translate(defense))
    percentages <- percentages %>%
      mutate(key = translate(key),
             defense = translate(defense),
             word = translate(word))
    x <- 'Testimonis'
    title <- 'Acceptació/Rebuig de testimonis agents de policia'
    subtitle <- 'Acusació vs. Defensa | Judici del procés'
    caption <- 'Joe Brew | @joethebrew | www.vilaweb.cat'
  } else {
    x <- 'Witnesses'
    title <- 'Acceptance and rejection of police witnesses'
    subtitle <- 'Prosecution vs. Defense | Trial of Catalan independence leaders'
    caption <- 'Joe Brew | @joethebrew | www.vilaweb.cat'
  }
  percentages <- percentages %>%
    group_by(defense) %>%
    mutate(label = 
             paste0(round(value, digits = 2), ' % ', word)) %>%
    summarise(label = paste0(label, collapse = '\n'))
  
  # plot_data$key <- factor(plot_data$key,
  #                         levels = rev(sort(unique(plot_data$key))))
  plot_data$key <- Hmisc::capitalize(plot_data$key)
  ggplot(data = plot_data,
         aes(x = defense,
             y = value)) +
    geom_bar(stat = 'identity',
             aes(fill = key)) +
    coord_flip() +
    theme_vilaweb()  +
    geom_text(data = percentages,
              aes(label = label,
                  y = 60),
              size = 4,
              alpha = 0.7,
              hjust = 0) +
    scale_fill_manual(name = '',
                      values = cols,
                      guide = guide_legend(reverse = TRUE)) +
    labs(x = '',
         y = x,
         title = title,
         subtitle = subtitle,
         caption = caption)
}
