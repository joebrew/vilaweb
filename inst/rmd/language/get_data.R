library(vilaweb)
library(rtweet)
library(tidyverse)
library(qdapRegex)
library(cld2)
library(zoo)

if('prepared_data.RData' %in% dir()){
  load('prepared_data.RData')
} else {
  # Define the people whose data is to be analyzed
  people <- c('InesArrimadas',
              'sanchezcastejon',
              'PSOE',
              'socialistes_cat',
              'CiutadansCs',
              'PPopular',
              'PPCatalunya',
              'Albert_Rivera',
              'Albiol_XG',
              'ALevySoler',
              'alejandroTGN',
              'vox_es',
              'ciudadanoscs',
              'pablocasado_',
              'miqueliceta',
              'Santi_ABASCAL',
              'Eva_Granados',
              'carrizosacarlos',
              'Societatcc',
              'carmencalvo_',
              'meritxell_batet',
              'JosepBorrellF',
              'ElMundoEspana',
              'elpais_espana',
              'LaVanguardia',
              'cronicaglobal',
              'elespanolcom',
              'elperiodico',
              'elconfidencial',
              'OKDIARIO',
              'enoticiescat',
              'publico_es','junqueras',
              'KRLS',
              'perearagones',
              'QuimTorraiPla',
              'JoanTarda',
              'Esquerra_ERC',
              'cupnacional',
              'Pdemocratacat',
              'CridaNacional',
              'ESPCiudadana',
              'marianorajoy',
              'rogertorrent',
              'XSalaimartin',
              'AlbanoDante76',
              'gabrielrufian',
              'carlescampuzano')
  
  # Get twitter data
  out_list <- list()
  for(p in 1:length(people)){
    this_person <- people[p]
    file_name <- (paste0('data/', this_person, '_tweets/tweets.csv'))
    if(!file.exists(file_name)){
      message(toupper(this_person), '----------------')
      system(paste0("python3 ../../../foreign/twint/Twint.py -u ",
                    this_person,
                    " -o data/",
                    this_person, 
                    "_tweets.csv --csv"))
    }
    # Read in the data
    tl <- read_csv(paste0('data/', (this_person), '_tweets/tweets.csv'))
    out_list[[p]] <- tl
  }
  # Combine all the different users data
  tl <- bind_rows(out_list)
  
  # There is now an object named tl, with all tweets
  # Ensure no duplicates
  tl <- tl %>% dplyr::distinct(.keep_all = TRUE) %>%
    filter(!duplicated(id))
  
  # Define function for cleaning tweet
  remove_url <- function(x){
    x <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", '', x)
    x <- rm_twitter_url(x)
    x <- rm_url(x)
    return(x)
  }
  remove_tags <- function(x){
    x <- gsub(' @\\S+', '', x)
    x <- gsub('#', '', x)
    x <- gsub(' pic.twitter.com\\S+', '', x)
    return(x)
  }
  clean_tweet <- function(x){
    remove_url(remove_tags(x))
  }
  # Get language
  tl$language <- NA # detect_language(clean_tweet(tl$tweet))
  tl$tweet_clean <- clean_tweet(tl$tweet)
  
  language_list <- list()
  for(i in 1:nrow(tl)){
    message(i, ' of ', nrow(tl))
    this_text <- tl$tweet_clean[i]
    this_language <- detect_language_mixed(this_text)
    language_list[[i]] <- this_language
  }
  save(language_list, file = 'language_list.RData')
  
  
  # From the language list, get codes
  extract_language <- function(ll){
    left <- data_frame(language = c('SPANISH',
                                    'OTHER',
                                    'CATALAN'))
    out <- ll$classification %>%
      mutate(language = ifelse(!language %in% c('SPANISH', 'CATALAN'),
                               'OTHER', language)) %>%
      group_by(language) %>%
      summarise(value = sum(proportion, na.rm = TRUE)) %>%
      ungroup 
    out <- left %>%
      left_join(out, by = 'language') %>%
      mutate(value = ifelse(is.na(value), 0, value)) %>%
      spread(key = language, value = value) %>%
      mutate(reliable = ll$reliabale) %>%
      mutate(language = ifelse(!reliable, 'OTHER',
                               ifelse(SPANISH > CATALAN, 'SPANISH',
                                      ifelse(CATALAN > SPANISH, 'CATALAN', 'OTHER'))))
  }
  
  out_list <- list()
  for(i in 1:nrow(tl)){
    message(i)
    out_list[[i]] <- lapply(language_list[i], extract_language)
  }
  
  # Make sure there are names
  has_names <- lapply(out_list, function(x){length(names(x)) > 0})
  has_names <- unlist(has_names)
  # save(out_list, file = 'temp.RData')
  x <- lapply(out_list, unlist)
  y <- do.call('rbind', x)
  z <- data.frame(y)
  z$CATALAN <- as.numeric(as.character(z$CATALAN))
  z$OTHER <- as.numeric(as.character(z$OTHER))
  z$SPANISH <- as.numeric(as.character(z$SPANISH))
  z$reliable <- as.logical(z$reliable)
  z$language <- as.character(z$language)
  
  tl$language <- NULL
  tl <- bind_cols(tl, z)
  
  # Save for faster loading
  save(tl, language_list, file = 'prepared_data.RData')
}



# tl$language <- detect_language(tl$tweet_clean)

make_plot <- function(language = 'en',
                      data = NULL,
                      time = 'week',
                      date_filter = c('2017-07-01', '2018-06-30'),
                      include_replies = FALSE){
  if(is.null(data)){
    data <- tl
  }
  data$date <- date_truncate(data$date, level = time)
  plot_data <- data
  if(!include_replies){
    plot_data <- plot_data %>% filter(is_reply_to == 0)
  }
  
  plot_data <- plot_data %>%
    mutate(language = ifelse(!language %in% c('ca', 'es'), 'other', language)) %>%
    filter(language != 'other')
  
  plot_data <- plot_data %>%
    group_by(date, 
             person = username,
             language) %>%
    summarise(n = n()) %>%
    ungroup %>%
    group_by(date, person) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup %>%
    # Remove last week (not finished)
    filter(date != max(date))
  # if(length(unique(plot_data$person)) == 1){
    left <- expand.grid(
      date = sort(unique(plot_data$date)),
      language = sort(unique(plot_data$language)),
      person = sort(unique(plot_data$person))
    )
    plot_data <- left_join(left, plot_data)
    plot_data$p[is.na(plot_data$p)] <- 0
    plot_data <- plot_data %>%
      group_by(person) %>%
      filter(date >= min(date[p > 0]))
  # }
  
  if(!is.null(date_filter)){
    plot_data <- plot_data %>%
      filter(date >= date_filter[1],
             date <= date_filter[2])
  }
  
  if(language == 'en'){
    x <- 'Week'
    y <- 'Percentage'
    title <- 'Language of tweets'
  } else {
    x <- 'Setmana'
    y <- 'Percentage'
    title <- 'Llengua de tuits'
  }
  
  cols <- databrew::make_colors(n = 20)[c(3,10)]
  ggplot(data = plot_data,
         aes(x = date,
             y = p,
             color = language)) +
    facet_wrap(~person, scales = 'free_x') +
    geom_line(alpha = 0.3) +
    geom_point(alpha = 0.3) +
    geom_smooth(se = FALSE, span = 0.2) +
    theme_vilaweb() +
    labs(x = x,
         y = y,
         title = title) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_color_manual(name = 'Language',
                       values = cols)
}
make_plot(data = tl %>% filter(username == 'inesarrimadas'),
          time = 'week',
          date_filter = c('2017-08-15', '2018-12-20'),
          include_replies = TRUE)

make_plot(data = tl %>% filter(username %in% c('inesarrimadas',
                                        'albiol_xg',
                                        # 'alevysoler',
                                        'carrizosacarlos',
                                        'ppcatalunya',
                                        'socialistes_cat',
                                        'societatcc')), time = 'month',
          date_filter = NULL)
