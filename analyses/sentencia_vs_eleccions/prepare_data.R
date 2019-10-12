library(vilaweb)
library(tidyverse)
library(databrew)
library(gsheet)

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


if('data.RData' %in% dir()){
  load('data.RData')
} else {
  judici_bcn <-
    rt <- search_tweets(
      '"sentència" OR "sentencia"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "41.385,2.173,20mi"
    )
  save(judici_bcn,
       file = 'judici_bcn.RData')
  judici_mad <-
    rt <- search_tweets(
      '"sentència" OR "sentencia"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "40.41678,-3.703,20mi"
    )
  save(judici_mad,
       file = 'judici_mad.RData')
  
  #
  elec_bcn <-
    rt <- search_tweets(
      '"elecciones" OR "eleccions"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "41.385,2.173,20mi"
    )
  save(elec_bcn,
       file = 'elec_bcn.RData')
  elec_mad <-
    rt <- search_tweets(
      '"elecciones" OR "eleccions"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "40.41678,-3.703,20mi"
    )
  save(elec_mad,
       file = 'elec_mad.RData')
  
  
  elec_sev <-
    rt <- search_tweets(
      '"elecciones" OR "eleccions"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "37.3891,-5.9845,20mi"
    )
  save(elec_sev,
       file = 'elec_sev.RData')
  judici_sev <-
    rt <- search_tweets(
      '"sentencia" OR "sentència"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "37.3891,-5.9845,20mi"
    )
  save(judici_sev,
       file = 'judici_sev.RData')
  
  
  
  
  elec_zar <-
    rt <- search_tweets(
      '"elecciones" OR "eleccions"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "41.6488,-0.8891,20mi"
    )
  save(elec_zar,
       file = 'elec_zar.RData')
  judici_zar <-
    rt <- search_tweets(
      '"sentencia" OR "sentència"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "41.6488,-0.8891,20mi"
    )
  save(judici_zar,
       file = 'judici_zar.RData')
  
  elec_val <-
    rt <- search_tweets(
      '"elecciones" OR "eleccions"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "39.4699,-0.3763,20mi"
    )
  save(elec_val,
       file = 'elec_val.RData')
  judici_val <-
    rt <- search_tweets(
      '"sentencia" OR "sentència"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "39.4699,-0.3763,20mi"
    )
  save(judici_val,
       file = 'judici_val.RData')
  
  
  
  save(elec_mad, elec_bcn, judici_mad, judici_bcn, elec_sev, judici_sev, elec_zar, judici_zar, elec_val, judici_val,
       file = 'data.RData')
}

df <- bind_rows(judici_bcn %>% mutate(city = 'Barcelona', subject = 'Sentencia/Sentència' ), 
                judici_mad %>% mutate(city = 'Madrid', subject = 'Sentencia/Sentència'),
                elec_bcn %>% mutate(city = 'Barcelona', subject = 'Eleccions/Elecciones' ),
                elec_mad %>% mutate(city = 'Madrid', subject = 'Eleccions/Elecciones'),
                judici_zar %>% mutate(city = 'Zaragoza', subject = 'Sentencia/Sentència' ), 
                judici_sev %>% mutate(city = 'Sevilla', subject = 'Sentencia/Sentència'),
                elec_zar %>% mutate(city = 'Zaragoza', subject = 'Eleccions/Elecciones' ),
                elec_sev %>% mutate(city = 'Sevilla', subject = 'Eleccions/Elecciones'))#,
                # elec_val %>% mutate(city = 'València', subject = 'Eleccions/Elecciones' ),
                # judici_val %>% mutate(city = 'València', subject = 'Sentencia/Sentència')
                # )

# Read in diputats del parlament
parlament <- get_google_data('parlament')
keep_names <- tolower(parlament$username[!is.na(parlament$username)])
people <- c(keep_names)

if(file.exists('tl.RData')){
  load('tl.RData')
} else {
  tl <- get_twitter_data_from_database(people = people)
  
  # Create variables for sentencia and elections
  tl <- tl %>%
    mutate(sentencia = grepl('sentència|sentencia', tolower(tweet)),
           eleccions = grepl('eleccions|elecciones', tolower(tweet)))
  
  tl <- tl %>% filter(username %in% people)
  tl <- tl %>% filter(!duplicated(id))
  
  # Join metadata
  tl <- left_join(tl,
                  parlament %>%
                    dplyr::select(username, partit))
  
  save(tl, file = 'tl.RData')  
}

# By party

by_party <- function(){
  pd <- tl %>%
    filter(date >= '2019-09-01') %>%
    # filter(!partit %in% c("CUP", "PP")) %>%
    group_by(partit) %>%
    summarise(tweets = n(),
              sentencia = length(which(sentencia)),
              eleccions = length(which(eleccions))) %>%
    ungroup %>%
    group_by(partit) %>%
    mutate(p_sentencia = sentencia / tweets * 100,
           p_eleccions = eleccions / tweets * 100) %>%
    mutate(sentencia_v_eleccions = p_sentencia / p_eleccions) %>%
    arrange(sentencia_v_eleccions)
    
  ggplot(data = pd,
         aes(x = ))    
}

zero_about_sentencia <- function(){
  pd <- tl %>%
    filter(date >= '2019-09-01') %>%
    # filter(!partit %in% c("CUP", "PP")) %>%
    group_by(username, partit) %>%
    summarise(tweets = n(),
              sentencia = length(which(sentencia)),
              eleccions = length(which(eleccions))) %>%
    ungroup %>%
    group_by(username, partit) %>%
    mutate(p_sentencia = sentencia / tweets * 100,
           p_eleccions = eleccions / tweets * 100) %>%
    mutate(sentencia_v_eleccions = p_sentencia / p_eleccions) %>%
    arrange(sentencia_v_eleccions)
  
}
