
# Define some functions
simplify_text <- function(x){
  x <- gsub("[\r\n]", "", x)
  x <- str_replace(gsub("\\s+", " ", str_trim(x)), "B", "b")
  return(x)
}

clean_text <- function(x){
  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- tolower(x)
  x <- simplify_text(x)
  return(x)
}

make_word_vector <- function(x){
  x <- clean_text(x)
  x_parsed <- strsplit(x, " ")
  x <- unlist(x_parsed)
}

make_word_df <- function(x){
  x <- make_word_vector(x)
  x <- data_frame(word = x)
  x <- x %>% group_by(word) %>% summarise(freq = n()) %>% arrange(desc(freq)) %>%
    mutate(cs = cumsum(freq)) %>%
    mutate(p = freq / sum(freq)) %>%
    mutate(psc = cs / sum(freq)) %>%
    filter(! word %in% c('', '\n'))
  # Calculate type token ratio
  # (types = total number of DIFFERENT words)
  # (tokens = total number of words)
  return(x)
}

# Define function for stemming
get_stem <- function(x, lang = NULL){
  stemmer <- vilaweb::stem
  if(!is.null(lang)){
    stemmer <- stemmer %>% filter(language == lang)
  } else {
    message('No lang provided. Will scan all languages.')
  }
  
  # Keep only the word in question
  find_stem <- function(y){
    out <- stemmer %>%
      filter(tolower(y) == original) %>%
      .$stem
    if(length(out) == 0){
      out <- y
    }
    if(length(out) > 1){
      out <- out[1]
    }
    return(out)
  }
  
  # Loop through each element of the vector
  out_list <- list()
  for(i in 1:length(x)){
    message('Finding stems for ', i , ' of ', length(x))
    # Get separated by spaces
    x_parsed <- unlist(strsplit(x[i], ' '))
    x_done <- unlist(lapply(x_parsed, find_stem))
    x_done <- paste0(x_done, collapse = ' ')
    out_list[[i]] <- x_done
  }
  out <- unlist(out_list)
  return(out)
}


# Make a score sentiment function
score_sentiment <- function (x, language = 'es',
                             valence_only = TRUE) {
  # Get the afinn and nrc dictionary
  af <- vilaweb::afinn
  nr <- vilaweb::nrc
  
  # Define the column for the right language
  word <- af %>% dplyr::select_(language)
  names(word) <- 'word'
  af$word <- word$word
  # Define the nr data for the right language
  if(language == 'en'){
    nr <- nrc %>% filter(lang == 'english')
  }
  if(language == 'ca'){
    nr <- nrc %>% filter(lang == 'catalan')
  }
  if(language == 'es'){
    nr <- nrc %>% filter(lang == 'spanish')
  }
  
  x <- clean_text(x)
  # Get stems
  message('Finding stems')
  x <- get_stem(x, lang = language)
  # Split at spaces
  x_parsed <- strsplit(x, " ")
  out <- rep(NA, length(x))
  nr_out <- list()
  for (i in 1:length(out)) {
    message('Scoring sentiment for ', i, ' of ', length(out))
    this_element <- x[i]
    this_element_parsed <- x_parsed[[i]]
    # Get af just for this
    af_small <- af %>% filter(word %in% this_element_parsed)
    if (nrow(af_small) == 0) {
      out[i] <- 0
    }
    else {
      out[i] <- mean(af_small$score, na.rm = TRUE)
    }
    
    # Get nr just for this
    nr_small <- nr %>% filter(word %in% this_element_parsed)
    nrc_names <- sort(unique(nrc$sentiment))
    nr_df <- data.frame(matrix(rep(0, length(nrc_names)), nrow = 1))
    names(nr_df) <- nrc_names
    if(nrow(nr_small) > 0){
      nr_scored <- nr_small %>%
        group_by(sentiment) %>%
        summarise(value = sum(value)) %>%
        ungroup
      for(j in 1:nrow(nr_scored)){
        nr_df[,nr_scored$sentiment[j]] <- nr_scored$value[j]
      }
    } 
    nr_out[[i]] <- nr_df
  }
  nr_out <- bind_rows(nr_out)
  nr_out$sentiment <- out
  if(valence_only){
    return(out)
  } else {
    return(nr_out)
  }
}

# Read in stopwords
catalan_spanish_stopwords <- 
  c(
    readLines('stopwords/stopwords-ca.txt'),
    readLines('stopwords/stopwords-es.txt')
  )
# Add a few
catalan_spanish_stopwords <- 
  c(catalan_spanish_stopwords,
    c('señor',
      'señoría',
      'señorías',
      'sánchez'))
# Remove numbers
numbers <- as.character(c(0:154,156:1935,1937:1977, 1979:2018))
catalan_spanish_stopwords <- c(catalan_spanish_stopwords, numbers)


library(tidyverse)
library(vilaweb)



# # Get people
# people <- c(
#   'sanchezcastejon',  'albert_rivera', 'inesarrimadas',
#             'pablocasado_', 'cayetanaat', 'igarrigavaz', 'santi_abascal',
#             'pablo_iglesias_',
#             'jaumeasens', 'krls', 'rogertorrent', 'junqueras',
#             'quimtorraipla', 'lauraborras', 'eva_granados',
#             'perearagones',
#             'miqueliceta')

library(RPostgreSQL)
pg = DBI::dbDriver("PostgreSQL")
con = DBI::dbConnect(pg, dbname = "twitter")
query = paste0("SELECT DISTINCT username FROM twitter")
people <- RPostgreSQL::dbGetQuery(con, query)
people <- people$username
query = paste0("SELECT * FROM twitter WHERE date > '2018-12-31'")
tl <- RPostgreSQL::dbGetQuery(con, query)
dbDisconnect(con)
# # Get their tweets
# tl <- get_twitter_data_from_database(people)



# Define function for getting the most tweeted word
word_freq <- function(data){
  tuits <- paste0(data$tweet, collapse = ' ')
  tuits_split <- strsplit(tuits, split = ' ') 
  tuits_vector <- unlist(tuits_split)
  tuits_vector <- tuits_vector[!grepl('@', tolower(tuits_vector), fixed = TRUE)]
  tuits_vector <- gsub('.', '', tuits_vector, fixed = TRUE)
  tuits_vector <- gsub(',', '', tuits_vector, fixed = TRUE)
  tuits_vector <- gsub(':', '', tuits_vector, fixed = TRUE)
  tuits_vector <- gsub(';', '', tuits_vector, fixed = TRUE)
  tuits_vector <- gsub('"', '', tuits_vector, fixed = TRUE)
  tuits_vector <- gsub('(', '', tuits_vector, fixed = TRUE)
  tuits_vector <- gsub(')', '', tuits_vector, fixed = TRUE)
  tuits_vector <- gsub('#', '', tuits_vector, fixed = TRUE)
  tuits_vector <- gsub('|', '', tuits_vector, fixed = TRUE)
  tuits_vector <- gsub('@', '', tuits_vector, fixed = TRUE)
  
  
  tuits_vector <- tolower(tuits_vector)
  tuits_df <- tibble(word = tuits_vector)
  en_stopwords <- c('the', 'of', 'to', 'and', 'in', '', 'for', '👇', '▶️', 'is', '-', '>', '🌊', '👉', '💪', '👉🏽')
  en_stopwords <- c(en_stopwords, '👉🏻', 'via', 'vía' )
  out <- tuits_df %>%
    filter(!word %in% catalan_spanish_stopwords,
           !word %in% en_stopwords) %>%
    group_by(word) %>%
    tally %>%
    arrange(desc(n)) %>%
    mutate(p = n / sum(n) * 100)
}

out_list <- list()
for(i in 1:length(people)){
  message(i)
  this_person <- people[i]
  this_data <- tl %>%
    filter(username == this_person,
           date >= '2019-01-01')
  outy <- word_freq(this_data)
  outy <- outy[1:10,] %>% mutate(username = this_person) %>%
    mutate(rank = 1:10)
  out_list[[i]] <- outy
}
out <- bind_rows(out_list)
out <- out %>% arrange(rank)
View(out)
write_csv(out, '2019.csv')
