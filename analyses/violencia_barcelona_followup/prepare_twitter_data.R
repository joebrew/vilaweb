library(tidyverse)
# Read in twitter credentials
library(yaml)
# twitter_credentials <- yaml.load_file('../../credentials/credentials.yaml')
# ## load rtweet package
# library(rtweet)
# token <- create_token(
#   app = "bcndata",
#   consumer_key = twitter_credentials$twitter_api_key,
#   consumer_secret = twitter_credentials$twitter_api_secret_key,
#   access_token = twitter_credentials$twitter_access_token,
#   access_secret = twitter_credentials$twitter_access_token_secret)
# 
# 
# if('data.RData' %in% dir()){
#   load('data.RData')
# } else {
#   violencia_bcn <-
#     rt <- search_tweets(
#       '("violencia" OR "violència" OR "inseguridad" OR "inseguritat" OR "delitos" OR "delictes" OR "crimen" OR "crim")', 
#       n = 1000000000, 
#       include_rts = F, 
#       retryonratelimit = TRUE,
#       geocode = "41.385,2.173,20mi"
#     )
#   save(violencia_bcn,
#        file = 'violencia_bcn.RData')
#   
#   
#   cerveza_bcn <-
#     rt <- search_tweets(
#       '("cervesa" OR "cerveza")', 
#       n = 1000000000, 
#       include_rts = F, 
#       retryonratelimit = TRUE,
#       geocode = "41.385,2.173,20mi"
#     )
#   save(cerveza_bcn,
#        file = 'cerveza_bcn.RData')
#   
#   violencia_mad <-
#     rt <- search_tweets(
#       '("violencia" OR "violència" OR "inseguridad" OR "inseguritat" OR "delitos" OR "delictes" OR "crimen" OR "crim")', 
#       n = 1000000000, 
#       include_rts = F, 
#       retryonratelimit = TRUE,
#       geocode = "40.41678,-3.703,20mi"
#     )
#   save(violencia_mad,
#        file = 'violencia_mad.RData')
#   
#   cerveza_mad <-
#     rt <- search_tweets(
#       '("cerveza" OR "cervesa")', 
#       n = 1000000000, 
#       include_rts = F, 
#       retryonratelimit = TRUE,
#       geocode = "40.41678,-3.703,20mi"
#     )
#   save(violencia_mad,
#        file = 'cerveza_mad.RData')
#   
#   save(violencia_bcn, violencia_mad, cerveza_bcn, cerveza_mad,
#        file = 'data.RData')
#   
# }     

if('twitter.RData' %in% dir()){
  load('twitter.RData')
} else {
  # # Get tweets
  # twint -s '("violencia" OR "violència" OR "inseguridad" OR "inseguretat" OR "delitos" OR "delictes" OR "crimen" OR "crim") AND ("Barcelona")' --since 2019-01-01 --until 2019-09-01 -o data/tweets --csv

df <- read_csv('data/tweets/tweets.csv') %>%
  filter(!duplicated(id)) %>%
  filter(date <= '2019-09-01')

  # Adjust for time zone
  library(lubridate)
  df$date_time <- as.POSIXct(paste0(df$date, ' ', df$time, ' ', '+0', df$timezone))
  Sys.setenv(TZ='CET')


  save(df, file = 'twitter.RData')
}

# Get newspapers
library(gsheet)
if(!'goog.RData' %in% dir()){
  goog_newspapers <- gsheet::gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1k6_AlqojK47MMqzuFYAzBnDfYXysmUgSseaKvHTb3W4/edit#gid=148951570', sheetid =  2)
  save(goog_newspapers,
       file = 'goog.RData')
} else {
  load('goog.RData')
}
newspapers <- tolower(goog_newspapers$newspaper)

make_keys <- function(x,y,z){
  return(data_frame(newspaper = x,
                    site = y,
                    handle = z))
}
keys <- 
  list(
    make_keys('La Vanguardia', 'lavanguardia.com', 'lavanguardia'),
    make_keys('El Mundo', 'elmundo.es', 'elmundoes'),
    make_keys('Telecinco', 'telecinco.es', 'telecincoes'),
    make_keys('El Diario', 'eldiario.es', 'eldiarioes'),
    make_keys('Huffington Post', 'huffingtonpost.es',''),
    make_keys('Público', 'publico.es', 'publico'),
    make_keys('El Periódico', 'elperiodico.es', 'elperiodico'),
    make_keys('El Periódico', 'elperiodico.com', 'elperiodico'),
    make_keys('Cadana Ser', 'cadenaser.com', 'cadanaser_espa'),
    make_keys('La Verdad', 'laverdad.es', 'laverdad_es'),
    make_keys('20 Minutos', '20minutos.es','20m'),
    make_keys('El Español', 'elespanol.com', 'elespanolcom'),
    make_keys('ABC', 'abc.es', 'abc_es'),
    make_keys('Crónica Global', 'cronicaglobal.com', 'cronicaglobal'),
    make_keys('CCMA/324', 'ccma.cat', 'ccmaa_cat'),
    make_keys('RTVE', 'rtve.es', 'rtve'),
    make_keys('ARA', 'ara.cat', 'diariaara'),
    make_keys('El Confidencial', 'elconfidencial.com', 'elconfidencial'),
    make_keys('Europa Press', 'europapress.es', 'europapress'),
    make_keys('E-noticies', 'e-noticies.cat', 'enoticiescat'),
    make_keys('E-noticies', 'e-noticies.es', 'enoticiescat'),
    make_keys('VilaWeb', 'vilaweb.cat', 'vilaweb'),
    make_keys('Libertad Digital', 'libertaddigital.com', 'libertaddigital'),
    make_keys('La Razón', 'larazon.es', 'larazon_es'),
    make_keys('El Punt Avui', 'elpuntavui.cat', 'elpuntavui'),
    make_keys('El Nacional', 'elnacional.cat', 'elnacionalcat'),
    make_keys('Nació Digital', 'naciodigital.cat', 'naciodigital'),
    make_keys('OKDiario', 'okdiario.com', 'okdiario'),
    make_keys('Voz Populi', 'vozpopuli.com', 'voz_populi'),
    make_keys('Onda Cero', 'ondacero.es', 'ondacero_es'),
    make_keys('El Economista', 'eleconomista.es', 'eleconomistaes'),
    make_keys('COPE', 'cope.es', 'cope'),
    make_keys('Diari de Girona', 'diaridegirona.cat', 'diaridegirona'),
    make_keys('elCatalán', 'elcatalan.es', 'elcatalan_es'),
    make_keys('324 Cat', '324.cat', '324cat'),
    make_keys('El País', 'elpais.com', 'elpais_espana'),
    make_keys('El País', 'elpais.com', 'elpais'),
    make_keys('El País', 'elpais.es', 'elpais_espana'),
    make_keys('Economia Digital', 'economiadigital.es', 'economiaed_'),
    make_keys('El Periódico', 'elperiodico.cat', 'elperiodico_cat'),
    make_keys('Diari la Veu', 'diarilaveu.com', 'diarilaveu_'),
    make_keys("L'independant", "lindependant.fr", "lindependant"),
    make_keys("Le Monde", "lemonde.fr", "lemondefr"),
    make_keys("Le Figaró", "lefigaro.fr", "le_figaro"),
    make_keys("Diario de Mallorca", "diariodemallorca.es", "diariomallorca"),
    make_keys("Última hora", "ultimahora.es", "uhmallorca"),
    make_keys("Diari de Balears", "dbalears.cat", "dbalears")
  )
keys <- bind_rows(keys)

# Manually combine some
keys <- keys %>%
  mutate(newspaper = ifelse(newspaper == 'Crónica Global',
                            'El Español', 
                            ifelse(newspaper == '324 Cat',
                                   'CCMA/324',
                                   newspaper)))

keys <- left_join(keys, goog_newspapers)
keys <- keys %>%
  filter(!handle %in% c('', 'le_figaro', 'lemondefr', 'economiaed_', 'diarilaveu_',
                        'lindependant', 'diariomallorca', 'uhmallorca', 'dbalears'))
newspapers <- rev(sort(unique(keys$handle)))
newspapers <- newspapers[newspapers != '']
# newspapers <- newspapers[8:length(newspapers)]
library(vilaweb)
# update_database(people = newspapers, delete_duplicates = FALSE)

# Get newspapers data
if('newspapers.RData' %in% dir()){
  load('newspapers.RData')
} else {
  pg = DBI::dbDriver("PostgreSQL")
  con = DBI::dbConnect(pg, dbname="twitter")
  query = paste0("SELECT * FROM twitter where username = ANY ('{",
                 paste0('"', newspapers, '"', collapse = ','),
                 "}')")
  tl <- RPostgreSQL::dbGetQuery(
    con,
    query
  )
  DBI::dbDisconnect(con)
  newspapers_df <- tl
  save(newspapers_df, file = 'newspapers.RData')
}
