# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)
library(rlang)

# Read in twitter credentials
library(yaml)
twitter_credentials <- yaml.load_file('../../credentials/credentials.yaml')
## load rtweet package
library(rtweet)
token <- create_token(
  app = "bcndata",
  consumer_key = twitter_credentials$twitter_api_key,
  consumer_secret = twitter_credentials$twitter_api_secret_key,
  access_token = twitter_credentials$twitter_access_token,
  access_secret = twitter_credentials$twitter_access_token_secret)


processed <- 'processed.RData'
if(processed %in% dir()){
  load(processed)
} else {
  # Get all diputats
  file_name <- 'diputats.RData'
  if(file_name %in% dir()){
    load(file_name)
  } else {
    diputats <- get_google_data('parlament')
    save(diputats, file = file_name)
  }
  
  people <- diputats$username
  people <- people[!is.na(people)]
  ## Ensure they are updated in the database
  # update_database(people = people)
  
  # Get their tweets
  if('tweets.RData' %in% dir()){
    load('tweets.RData')
  } else {
    tl <- get_twitter_data_from_database(people = people)
    save(tl, file = 'tweets.RData')
  }
  
  # Generate helper variables
  tl <- tl %>%
    mutate(month = as.Date(cut(date, 'month'))) %>%
    mutate(year = as.numeric(format(date, '%Y'))) 
  
  get_mentions <- function(z){
    z <- gsub('[', '', z, fixed = T)
    z <- gsub(']', '', z, fixed = T)
    z <- gsub("'", '', z, fixed = T)
    z <- gsub(' ', '', z)
    # z <- trimws(z, which = 'both')
    z <- strsplit(z, split = ',')
    return(z)
  }
  all_mentions <- get_mentions(tl$mentions)
  
  # Generate tag variables
  ll <- setNames(rep(NA, length(people)), as.list(people))
  tl <- tl %>% mutate( !!! ll )
  for(j in 1:length(people)){
    message(j, ' of ', length(people))
    person <- people[j]
    out = lapply(all_mentions, function(x){person %in% x})
    tl[,person] <- unlist(out)
  }
  
  # Join the political party info
  tl <- tl %>%
    dplyr::rename(twitter_name = name) %>%
    left_join(diputats)
  
  # Get who follows who
  followers <- diputats$username[!is.na(diputats$username)]
  followers_list <- list()
  for(i in 1:length(followers)){
    message(i, ' of ', length(followers))
    this_person <- followers[i]
    fds <- get_friends(this_person, retryonratelimit = TRUE)
    while(nrow(fds) == 0){
      sleeper <- rnorm(mean = 300, sd = 100, n = 1)
      sleeper <- ifelse(sleeper < 50, 100, sleeper)
      message('Sleeping ', sleeper, ' seconds')
      Sys.sleep(time = sleeper)
      fds <- get_friends(this_person, retryonratelimit = TRUE)
    }
    followers_list[[i]] <- fds
    save(followers_list, i, followers, file = 'temp.RData')
  }
  followers <- bind_rows(followers_list)
  
  # Get info on the diputats
  diputats_info <- lookup_users(people, parse = TRUE, token = NULL)
  diputats_info <- diputats_info %>%
    dplyr::select(user_id,
                  screen_name,
                  name,
                  location,
                  description,
                  url,
                  protected,
                  followers_count,
                  friends_count,
                  listed_count,
                  statuses_count,
                  favourites_count,
                  account_created_at,
                  verified,
                  profile_url,
                  account_lang) %>%
    mutate(user_name = tolower(screen_name))
  
  save(tl, all_mentions, diputats, people, diputats_info,
       file = processed)
}



#

# Vilaweb theme
theme_vilaweb_date <- 
  function (base_size = 15, base_family = "Avenir", y_comma = TRUE, 
          subtitle_family = "Avenir", axis_family = "Avenir") 
{
  color_background = "white"
  color_grid_major = "grey"
  color_axis_text = "#333333"
  color_axis_title = color_axis_text
  color = "darkgrey"
  color_title = "#FF5B04"
  color_subtitle = "#3C3C3C"
  color_caption = "black"
  base_size1 = base_size
  out <- theme_bw(base_size = base_size1) + theme(panel.background = element_rect(fill = color_background, 
                                                                                  color = color_background)) + theme(plot.background = element_rect(fill = color_background, 
                                                                                                                                                    color = color_background)) + theme(panel.border = element_rect(color = color_background)) + 
    theme(panel.grid.major = element_line(color = adjustcolor(color_grid_major, 
                                                              alpha.f = 0.25), size = 0.25)) + theme(panel.grid.major = element_line(color = adjustcolor(color_grid_major, 
                                                                                                                                                         alpha.f = 0.4), size = 0.4)) + theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) + theme(legend.background = element_rect(fill = color_background)) + 
    theme(legend.text = element_text(family = base_family, 
                                     size = base_size * 0.7, color = color_axis_title)) + 
    theme(plot.title = element_text(family = base_family, 
                                    color = color_title, size = base_size * 1.2, vjust = 1.25)) + 
    theme(plot.subtitle = element_text(family = subtitle_family, 
                                       color = color_subtitle, size = base_size * 0.8, vjust = 1.25)) + 
    theme(axis.text.x = element_text(family = axis_family, 
                                     size = base_size * 0.62, color = color_axis_text)) + 
    theme(axis.text.y = element_text(family = axis_family, 
                                     size = base_size * 0.7, color = color_axis_text)) + 
    theme(axis.title.x = element_text(family = axis_family, 
                                      size = base_size * 0.9, color = color_axis_title, 
                                      vjust = 0)) + theme(axis.title.y = element_text(family = axis_family, 
                                                                                      size = base_size * 0.9, color = color_axis_title, vjust = 1.25)) + 
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) + 
    theme(complete = TRUE) + theme(legend.title = element_text(family = axis_family, 
                                                               size = base_size, color = color_axis_text)) + theme(legend.position = "bottom") + 
    theme(strip.background = element_rect(color = NA, fill = NA), 
          strip.text = element_text(size = base_size * 0.9, 
                                    family = base_family), panel.spacing = unit(0, 
                                                                                "lines"), panel.border = element_rect(colour = NA, 
                                                                                                                      fill = NA, size = 0), plot.caption = element_text(size = base_size * 
                                                                                                                                                                          0.7, family = base_family, color = color_caption))
  if (y_comma) {
    out <- list(out, scale_y_continuous(label = scales::comma))
  }
  else {
    out <- list(out)
  }
  return(out)
}