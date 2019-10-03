update <- FALSE

# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

file_name <- 'tl.RData'

if(!file_name %in% dir()){
  # Get usernames of politicians
  socialists <- get_google_data(name = 'socialists')
  psc <- get_google_data(name = 'parlament')
  
  # Combine
  socialists <- socialists %>%
    filter(!is.na(title))
  psc <- psc %>% filter(partit == 'PSC')
  socialists <- bind_rows(socialists, psc)
  
  # Remove those with no twitter account
  socialists <- socialists %>%
    filter(!is.na(username))
  
  # Define the (lowercase) database versions
  these_names <- tolower(unique(socialists$username))
  if(update){
    update_database(people = these_names)
  }
  
  # Get tweets
  tl <- get_twitter_data_from_database(people = these_names)
  
  # Mark catalan
  tl$psc <- tolower(tl$username) %in% tolower(psc$username)
  
  # Define references to the right
  tl$right <- 
    grepl('derecha|dreta|dretes', tolower(tl$tweet))
  tl$violence <-
    grepl('violència|violencia|terroris|cdr|atac|ataq|bomba|explosi', tolower(tl$tweet))
  # Set dates
  tl$year_month <- as.Date(cut(tl$date, 'month'))
  tl$year_week <- as.Date(cut(tl$date, 'week'))
  # Filter for only through end of September
  tl <- tl %>% filter(date <= '2019-09-30')
  
  # Get PSOE and PSC
  tl_party <- get_twitter_data_from_database(people = c('psoe', 'socialistes_cat'))
  tl_party$right <- 
    grepl('derecha|dreta|dretes', tolower(tl_party$tweet))
  tl_party$year_month <- as.Date(cut(tl_party$date, 'month'))
  tl_party$year_week <- as.Date(cut(tl_party$date, 'week'))
  tl_party <- tl_party %>% filter(date <= '2019-09-30')
  tl_party$violence <-
    grepl('violència|violencia|terroris|cdr|atac|ataq|bomba|explosi', tolower(tl_party$tweet))
  
  tl <- tl %>% filter(username != 'maximhuerta')
  socialists <- socialists %>% filter(username != 'maximhuerta')
  
  save(tl, socialists, tl_party, file = file_name)
} else {
  load(file_name)
}

# Define catalan date maker
cat_date <- function (x, new_line = FALSE, month = T){
  part2 <- format(x, "%d")
  part3 <- format(x, "%Y")
  part1 <- data.frame(x = as.numeric(format(x, "%m")))
  right <- data_frame(x = 1:12, y = c("Jan", "Feb", "Mar", 
                                      "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                                      "Dec"))
  joined <- left_join(part1, right) %>% .$y
  out <- paste0(part2, " ", joined, " ", part3)
  if (!month) {
    return(out)
  }
  else {
    out <- substr(out, 4, nchar(out))
    if (new_line) {
      out <- gsub(" ", "\n", out)
    }
    return(out)
  }
}

# Define function for plotting frequency by month
make_pre_election_plot <- function(){
  pd <- tl %>%
    filter(date <= '2019-04-30',
           date >= '2015-01-01') 
  nr <- nrow(pd)
  pd <- pd %>%
    group_by(date = year_month, right) %>%
    tally %>%
    mutate(p = n / sum(n) * 100) %>%
    filter(right)
  left <- tibble(date = seq(min(pd$date),
                            max(pd$date),
                            by = 'month'))
  pd <- left_join(left, pd)
  pd$p[is.na(pd$p)] <- 0
  ggplot(data = pd,
         aes(x = date,
             y = p)) +
    geom_area(fill = colors_vilaweb()[3],
              color = 'black',
              alpha = 0.8) +
    # geom_bar(stat = 'identity') +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1,
                                     size = 6)) +
    scale_x_date(breaks = sort(unique(pd$date)),
                 labels = cat_date(sort(unique(pd$date)))) +
    labs(title = "Tweets mentioning the 'right' from socialist politicians",
         subtitle = 'Percentage by month. 17 PSC parlementarians, 14 PSOE Ministers, and Pedro Sánchez',
         caption = paste0(numberfy(nr), " tweets from 32 socialists. Monthly frequency of the following words: 'dreta', 'dretes', 'derecha', 'derechas'.\nChart by @joethebrew. www.vilaweb.cat"),
         x = 'Month') +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = 0.5,
              size = 2.5,
              alpha = 0.7) +
    scale_y_continuous(name = 'Percentage', breaks = 0:100)
}

# Plot for separating by person
# Define function for plotting frequency by month
make_pre_election_plot_person <- function(){
  pd <- tl %>%
    filter(date <= '2019-04-30',
           date >= '2018-01-01') 
  nr <- nrow(pd)
  pd <- pd %>%
    group_by(date = year_month,
             username,
             name,
             right) %>%
    tally %>%
    mutate(p = n / sum(n) * 100) %>%
    filter(right)
  # Keep only the most well-known socialists
  pd <- pd %>%
    filter(username %in% c('miqueliceta',
                           'eva_granados',
                           'abalosmeco',
                           'meritxell_batet',
                           'sanchezcastejon',
                           'josepborrellf'))
  left <- expand.grid(username = sort(unique(pd$username)),
                 date = seq(min(pd$date),
                            max(pd$date),
                            by = 'month'))
  pd <- left_join(left, pd)
  pd$p[is.na(pd$p)] <- 0
  pd$name <- gsub(' /❤️', '', pd$name)
  pd <- pd %>%
    group_by(username) %>%
    mutate(thename = dplyr::first(name[!is.na(name)]))
  pd$name <- pd$thename
  pd <- pd %>% filter(!is.na(name))
  ggplot(data = pd,
         aes(x = date,
             y = p)) +
    # geom_area(fill = colors_vilaweb()[3],
    #           color = 'black',
    #           alpha = 0.8) +
    facet_wrap(~name,
               scales = 'free_y') +
    geom_bar(stat = 'identity',
             fill = colors_vilaweb()[4]) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1,
                                     size = 6)) +
    scale_x_date(breaks = sort(unique(pd$date)),
                 labels = cat_date(sort(unique(pd$date)))) +
    
    labs(title = "Tweets mentioning the 'right' from socialist politicians",
         subtitle = 'Percentage by month',
         caption = paste0("Monthly frequency of the following words: 'dreta', 'dretes', 'derecha', 'derechas'.\nChart by @joethebrew. www.vilaweb.cat"),
         x = 'Month',
         y = 'Percentage') +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = 0.5,
              size = 2,
              alpha = 0.7) 
    # scale_y_continuous(name = 'Percentatge', breaks = 0:100)
}


make_post_election_plot <- function(){
  pd <- tl %>%
    filter(date <= '2019-09-30',
           date >= '2018-01-01') 
  nr <- nrow(pd)
  pd <- pd %>%
    group_by(date = year_month, right) %>%
    tally %>%
    mutate(p = n / sum(n) * 100) %>%
    filter(right)
  left <- tibble(date = seq(min(pd$date),
                            max(pd$date),
                            by = 'month'))
  pd <- left_join(left, pd)
  pd$p[is.na(pd$p)] <- 0
  ggplot(data = pd,
         aes(x = date,
             y = p)) +
    geom_bar(fill = colors_vilaweb()[5],
             stat = 'identity',
              color = 'black',
              alpha = 0.8) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1,
                                     size = 6)) +
    scale_x_date(breaks = sort(unique(pd$date)),
                 labels = cat_date(sort(unique(pd$date)))) +
    labs(title = "Tweets mentioning the 'right' from socialist politicians",
         subtitle = 'Percentage by month. 17 PSC parlementarians, 14 PSOE Ministers, and Pedro Sánchez',
         caption = paste0(numberfy(nr), " tweets from 32 socialists. Monthly frequency of the following words: 'dreta', 'dretes', 'derecha', 'derechas'.\nChart by @joethebrew. www.vilaweb.cat"),
         x = 'Month') +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = 0.5,
              size = 2.5,
              alpha = 0.7) +
    scale_y_continuous(name = 'Percentage', breaks = 0:100) +
    geom_vline(xintercept = as.Date('2019-04-12'),
               color = 'red') +
    geom_label(data = data.frame(date = as.Date('2019-04-12'),
                                 p = 9,
                                 label = '28-A'),
               aes(x = date,
                   y = p,
                   label = label),
               nudge_x = 20,
               color = 'red')
}

make_post_election_plot_person <- function(){
  pd <- tl %>%
    filter(date <= '2019-09-30',
           date >= '2018-01-01') 
  nr <- nrow(pd)
  pd <- pd %>%
    group_by(date = year_month,
             username,
             name,
             right) %>%
    tally %>%
    mutate(p = n / sum(n) * 100) %>%
    filter(right)
  # Keep only the most well-known socialists
  pd <- pd %>%
    filter(username %in% c('miqueliceta',
                           'eva_granados',
                           'abalosmeco',
                           'meritxell_batet',
                           'sanchezcastejon',
                           'josepborrellf'))
  left <- expand.grid(username = sort(unique(pd$username)),
                      date = seq(min(pd$date),
                                 max(pd$date),
                                 by = 'month'))
  pd <- left_join(left, pd)
  pd$p[is.na(pd$p)] <- 0
  pd$name <- gsub(' /❤️', '', pd$name)
  pd <- pd %>%
    group_by(username) %>%
    mutate(thename = dplyr::first(name[!is.na(name)]))
  pd$name <- pd$thename
  pd <- pd %>% filter(!is.na(name))
  ggplot(data = pd,
         aes(x = date,
             y = p)) +
    # geom_area(fill = colors_vilaweb()[3],
    #           color = 'black',
    #           alpha = 0.8) +
    facet_wrap(~name,
               scales = 'free_y') +
    geom_bar(stat = 'identity',
             fill = colors_vilaweb()[4]) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1,
                                     size = 6)) +
    scale_x_date(breaks = sort(unique(pd$date)),
                 labels = cat_date(sort(unique(pd$date)))) +
    labs(title = "Tweets mentioning the 'right' from socialist politicians",
         subtitle = 'Percentage by month',
         caption = paste0("Monthly frequency of the following words: 'dreta', 'dretes', 'derecha', 'derechas'.\nChart by @joethebrew. www.vilaweb.cat"),
         x = 'Month',
         y = 'Percentage') +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = 0.5,
              size = 2,
              alpha = 0.7) +
    geom_vline(xintercept = as.Date('2019-04-12'),
               color = 'red') 
  # scale_y_continuous(name = 'Percentatge', breaks = 0:100)
}



make_post_election_plot_party <- function(){
  pd <- tl_party %>%
    filter(date <= '2019-09-30',
           date >= '2018-01-01') 
  nr <- nrow(pd)
  pd <- pd %>%
    group_by(date = year_month,
             username,
             right) %>%
    summarise(n = n(),
              name = dplyr::first(name)) %>%
    mutate(p = n / sum(n) * 100) %>%
    filter(right)
  left <- expand.grid(name = sort(unique(pd$name)),
                      date = seq(min(pd$date),
                                 max(pd$date),
                                 by = 'month'))
  pd <- left_join(left, pd)
  pd$p[is.na(pd$p)] <- 0
  pd$name <- gsub(' /❤️', '', pd$name)
  pd <- pd %>%
    group_by(name) %>%
    mutate(thename = dplyr::first(name[!is.na(name)]))
  pd <- pd %>% filter(!is.na(name))
  ggplot(data = pd,
         aes(x = date,
             y = p)) +
    # geom_area(fill = colors_vilaweb()[3],
    #           color = 'black',
    #           alpha = 0.8) +
    facet_wrap(~name) +
    geom_bar(stat = 'identity',
             fill = colors_vilaweb()[4]) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1,
                                     size = 6)) +
    scale_x_date(breaks = sort(unique(pd$date)),
                 labels = cat_date(sort(unique(pd$date)))) +
    labs(title = "Tweets mentioning the 'right' from official party Twitter accounts",
         subtitle = 'Percentage by month',
         caption = paste0("Monthly frequency of the following words: 'dreta', 'dretes', 'derecha', 'derechas'.\nChart by @joethebrew. www.vilaweb.cat"),
         x = 'Month',
         y = 'Percentage') +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = 0.5,
              size = 2.5,
              alpha = 0.7) +
    geom_vline(xintercept = as.Date('2019-04-12'),
               color = 'red') +
    geom_label(data = data.frame(date = as.Date('2019-04-12'),
                                 p = 27,
                                 label = '28-A'),
               aes(x = date,
                   y = p,
                   label = label),
               nudge_x = 20,
               color = 'red')
  # scale_y_continuous(name = 'Percentatge', breaks = 0:100)
}
