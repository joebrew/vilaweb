update <- FALSE

# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

file_name <- 'tl.RData'

if(!file_name %in% dir()){
  # Get usernames of politicians
  parlament <- get_google_data(name = 'parlament')
  
  # Combine
  parlament <- parlament %>%
    # Remove those with no twitter account
    filter(!is.na(username)) %>%
    mutate(username = tolower(username))
  
  # Define the (lowercase) database versions
  these_names <- tolower(unique(parlament$username))
  if(update){
    update_database(people = these_names)
  }
  
  # Get tweets
  tl <- get_twitter_data_from_database(people = parlament$username)
  
  # Define references to rey
  tl$rey <- 
    grepl('rey|rei|felipe', tolower(tl$tweet))
  
  # Define references to 155
  tl$cent <- 
    grepl(' 155|155ya', tolower(tl$tweet))
  
  # Get references to torra
  tl$torra <- grepl('torra | torra|quimtorraipla', tolower(tl$tweet))
  tl$roldan <- grepl('roldán | roldán|lroldansu', tolower(tl$tweet))
  
  # Set dates
  tl$year_month <- as.Date(cut(tl$date, 'month'))
  tl$year_week <- as.Date(cut(tl$date, 'week'))
  # # Filter for only through end of September
  # tl <- tl %>% filter(date <= '2019-09-30')
  
  # Get party
  tl <- tl %>%
    left_join(parlament %>% dplyr::select(username, partit))
  
  save(tl, file = file_name)
} else {
  load(file_name)
}

# Get screenshots
library(webshot)

# Get king tweets
king_tweets <- tl %>%
  filter(date >= '2019-10-03',
         rey)



# Define catalan date maker
cat_date <- function (x, new_line = FALSE, month = T){
  part2 <- format(x, "%d")
  part3 <- format(x, "%Y")
  part1 <- data.frame(x = as.numeric(format(x, "%m")))
  right <- data_frame(x = 1:12, y = c("Gen", "Feb", "Mar", 
                                      "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Oct", "Nov", 
                                      "Des"))
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

# Plot for separating by person
# Define function for plotting frequency by month
make_plot <- function(){
  pd <- tl %>%
    filter(date <= '2019-09-30',
           date >= '2019-01-01')
  nr <- nrow(pd)
  pd <- pd %>%
    group_by(date = year_month,
             partit,
             rey) %>%
    tally %>%
    mutate(p = n / sum(n) * 100) %>%
    filter(rey)
  pd <- pd %>%
    dplyr::select(date, partit, rey, n, p) %>%
    dplyr::rename(key = rey)
  
  
 
  left <- expand.grid(partit = sort(unique(pd$partit)),
                      key = sort(unique(pd$key)),
                      date = seq(min(pd$date),
                                 max(pd$date),
                                 by = 'month'))
  pd <- left_join(left, pd)
  pd$p[is.na(pd$p)] <- 0
   ggplot(data = pd,
         aes(x = date,
             y = p)) +
    # geom_area(fill = colors_vilaweb()[3],
    #           color = 'black',
    #           alpha = 0.8) +
    facet_grid(key~partit) +
    geom_bar(stat = 'identity') +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1,
                                     size = 6)) +
    scale_x_date(breaks = sort(unique(pd$date)),
                 labels = cat_date(sort(unique(pd$date)))) +
    labs(title = "Piulets de diputats del Parlament esmentant '155'",
         subtitle = 'Percentatge per mes',
         caption = paste0("Freqüència mensual. \nGràfic de @joethebrew. www.vilaweb.cat"),
         x = 'Mes',
         y = 'Percentatge') +
    geom_text(aes(label = round(p, digits = 1),
                  y = p + 1),
              size = 2,
              alpha = 0.7)  +
     scale_fill_manual(name = 'Esments de',
                       values = c(colors_vilaweb()[c(3,5)]))
  # scale_y_continuous(name = 'Percentatge', breaks = 0:100)
}
