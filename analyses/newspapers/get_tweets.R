library(vilaweb)
library(rtweet)
library(tidyverse)
library(qdapRegex)
library(cld2)
library(zoo)

if('tl.RData' %in% dir()){
  load('tl.RData')
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
              # 'ElMundoEspana',
              # 'elpais_espana',
              # 'LaVanguardia',
              # 'cronicaglobal',
              # 'elespanolcom',
              # 'elperiodico',
              # 'elconfidencial',
              # 'OKDIARIO',
              # 'enoticiescat',git sta
              # 'publico_es',
              'junqueras',
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
              'carlescampuzano',
              'ximopuig', 'monicaoltra', 'compromis', 'enricmorera', 'joanribo', 'joanbaldovi', 'F_Armengol', 'adacolau', 'toninoguera', 'isabelbonig', 'vicentsolerm', 'JeanMarcPujol', 'MESperMallorca', 'MesperMenorca', 'SocialistesVAL', 'CatenComu_Podem', 'GabrielRufian_',
              'G_Pisarello', 'Jaumeasens', 'Pablo_Iglesias_', 'pnique')
  
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
  
  # Define function for extracting urls
  extract_host <- function(x){
    if(grepl('twitter.com',x, fixed = TRUE)){
      out <- unlist(lapply(strsplit(x, '/', fixed = TRUE), function(x){x[4]}), use.names = TRUE)
    } else {
      host <- parse_url(x)$hostname
      if(is.null(host)){
        out <- NA
      } else {
        domain.info <- tldextract(host)
        out <- paste(domain.info$domain, domain.info$tld, sep=".")
      }
    }
    
    return(as.character(out))
  }
  # extract_host <- Vectorize(extract_host)
  
  # Get hosts
  urls <- tl$urls
  urls <- gsub('[', '', urls, fixed = TRUE)
  urls <- gsub(']', '', urls, fixed = TRUE)
  urls <- gsub("'", '', urls, fixed = TRUE)
  # Just keep the first
  urls <- unlist(lapply(strsplit(urls, ','), function(x){x[1]}))
  tl$host <- NA
  has_url <- which(!is.na(urls))
  lu <- length(urls)
  # has_url <- has_url[has_url >= 784828]
  for(i in has_url){
    message(i, ' of ', lu)
    done <- extract_host(urls[i])
    # print(done)
    tl$host[i] <- done
  }
  
  save(tl, file = 'tl.RData')
}

tl <- tl %>% filter(!username %in% c('xsalaimartin', 'jeanmarcpujol', 'gabrielrufian_'))
tl <- tl %>% filter(!is.na(username))

# Read in google sheet with keys
library(gsheet)
if(!'goog.RData' %in% dir()){
  goog_people <- gsheet::gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1k6_AlqojK47MMqzuFYAzBnDfYXysmUgSseaKvHTb3W4/edit#gid=1425313388')
  goog_newspapers <- gsheet::gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1k6_AlqojK47MMqzuFYAzBnDfYXysmUgSseaKvHTb3W4/edit#gid=148951570')
  save(goog_people,
       goog_newspapers,
       file = 'goog.RData')
} else {
  load('goog.RData')
}

make_keys <- function(x,y,z){
  return(data_frame(newspaper = x,
                    site = y,
                    handle = z))
}
# Extract core
# Define the url keys
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
    make_keys('VilaWeb', 'vilaweb.cat', 'VilaWeb'),
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

# Make a people keys data frame
people_keys <- 
  data_frame(username = sort(unique(tl$username))) %>%
  left_join(goog_people)

# Flag the tweets
tl$is_site <- !is.na(tl$host) & grepl('.', tl$host, fixed = TRUE)
tl$is_handle <- !is.na(tl$host) & !grepl('.', tl$host, fixed = TRUE)

# Lowercase all
tl$host <- tolower(tl$host)
keys$site <- tolower(keys$site)
keys$handle <- tolower(keys$handle)

# Keep only the data with newspaper references
sites <- tl %>% filter(is_site)
sites <- left_join(sites,
                   keys %>% dplyr::select(newspaper, site),
                   by = c('host' = 'site'))
handles <- tl %>% filter(is_handle)
handles <- left_join(handles,
                   keys %>% dplyr::select(newspaper, handle),
                   by = c('host' = 'handle'))
df <- bind_rows(sites, handles)

# Join the keys
df <- left_join(df, keys)
df <- left_join(df, people_keys)

# Take out the radios
df <- df %>%
  filter(!newspaper %in% c('Cadana Ser',
                           'CCMA/324',
                           'COPE',
                           'Onda Cero',
                           'Última hora',
                           'RTVE'))



# Create plot
create_plot <- function(language = 'ca',
                        usernames = NULL,
                        n = NULL,
                        switch = FALSE){
  if(!is.null(usernames)){
    plot_data <- 
      df %>%
      filter(username %in% usernames)
  } else {
    plot_data <- df
  }
  plot_data <- plot_data %>%
    # mutate(newspaper = gsub(' ', '\n', newspaper)) %>%
    mutate(person = username) %>%
    dplyr::group_by(person,
                    newspaper) %>%
    tally %>%
    ungroup %>%
    group_by(person) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup %>%
    arrange(person,
            desc(p)) %>%
    dplyr::filter(!is.na(newspaper)) 
  only_one <- length(usernames) == 1
  if(only_one){
    plot_data$newspaper <- factor(plot_data$newspaper,
                                  levels = plot_data$newspaper)
  }
  if(!is.null(n)){
    if(nrow(plot_data) > 10){
      plot_data <- plot_data[1:n,]
    }
  }
  
  ny <- max(plot_data$p, na.rm = TRUE) / 30
  
  g <- ggplot(data = plot_data,
         aes(x = newspaper,
             y = p)) +
    geom_bar(stat = 'identity',
             alpha = 0.6,
             fill = 'darkorange')
  if(length(usernames) != 1){
    g <- g + facet_wrap(~person)
  } 
  the_list <- 
    paste0("20 Minutos, ABC, ARA, Cadana Ser, CCMA, CCMA/324, COPE, Diari de Balears, Diari de Girona, Diari la Veu, Diario de Mallorca,\nE-noticies, Economia Digital, El Confidencial, El Diario, El Economista, El Español, El Mundo, El Nacional, El País, \nEl Periódico, El Público, El Punt Avui, elCatalán, Europa Press, Huffington Post, L'independant, La Razón, La Vanguardia, \nLa Verdad, Le Figaró, Le Monde, Libertad Digital, Nació Digital, OKDiario, Onda Cero, RTVE, Telecinco, Última hora, VilaWeb, Voz Populi")
  if(language == 'en'){
    x <- 'Source'
    title <- 'Distribution of newspaper references (top 10)'
    caption <- paste0('Includes the following only: ', the_list)
  } else {
    x <- 'Font'
    title <- 'Distribució de referencies a diaris (top 10)'
    caption <- paste0('Només inclou: ', the_list)
    
  }
  subtitle <- ifelse(only_one, paste0('@', plot_data$person[1]), '')
  y <- 'Percentage*'
  
  if(switch){
    a <- subtitle
    b <- title
    title <- a
    subtitle <- ''
    caption <- ''
  }
  g <- g +
    theme_vilaweb() +
    labs(x = x, y = y, title = title, subtitle = subtitle, caption = caption) +
    theme(plot.subtitle = element_text(size = 24, hjust = 0.5),
          plot.caption = element_text(size = 7)) +
    geom_text(aes(label = round(p, digits = 2)),
               nudge_y = ny,
              alpha = 0.6,
              size = 3) +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1))
  
  return(g)
}

# Create plot
create_plot_newspaper <- function(language = 'ca',
                        newspapers = NULL,
                        n = NULL,
                        switch = FALSE){
  plot_data <- df
  
  plot_data <- plot_data %>%
    # mutate(newspaper = gsub(' ', '\n', newspaper)) %>%
    mutate(person = username) %>%
    dplyr::group_by(person,
                    newspaper) %>%
    tally %>%
    ungroup %>%
    group_by(person) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup %>%
    arrange(person,
            desc(p)) %>%
    dplyr::filter(!is.na(newspaper)) 
  if(!is.null(newspapers)){
    plot_data <- 
      plot_data %>%
      filter(newspaper %in% newspapers)
  }
  plot_data <- plot_data %>% arrange(desc(p))
  only_one <- length(newspapers) == 1
  if(only_one){
    plot_data$person <- factor(plot_data$person,
                                  levels = plot_data$person)
  }
  if(!is.null(n)){
    if(nrow(plot_data) > 10){
      plot_data <- plot_data[1:n,]
    }
    
  }
  ny <- max(plot_data$p, na.rm = TRUE) / 30
  
  plot_data <- plot_data %>%
    filter(!is.na(person))
  g <- ggplot(data = plot_data,
              aes(x = person,
                  y = p)) +
    geom_bar(stat = 'identity',
             alpha = 0.6,
             fill = 'darkorange')
  if(length(newspapers) != 1){
    g <- g + facet_wrap(~newspaper)
  } 
  the_list <- 
    paste0("adacolau, albanodante76, albert_rivera, albiol_xg, alejandrotgn, alevysoler, carlescampuzano, carmencalvo_, carrizosacarlos,\ncatencomu_podem, ciudadanoscs, ciutadanscs, compromis, cridanacional, cupnacional, enricmorera, espciudadana, esquerra_erc,\neva_granados, f_armengol, gabrielrufian, gabrielrufian_, inesarrimadas, isabelbonig, jeanmarcpujol, joanbaldovi, joanribo, joantarda,\njosepborrellf, junqueras, krls, marianorajoy, meritxell_batet, mespermallorca, mespermenorca, miqueliceta, monicaoltra, pablocasado_,\npdemocratacat, perearagones, ppcatalunya, ppopular, psoe, quimtorraipla, rogertorrent, sanchezcastejon, santi_abascal, socialistes_cat,\nsocialistesval, societatcc, toninoguera, vicentsolerm, vox_es, ximopuig")
  if(language == 'en'){
    x <- 'Twitter account'
    title <- 'Distribution of newspaper references (top 10)'
    caption <- paste0('Includes the following twitter accounts only:\n', the_list)
  } else {
    x <- 'Compte twitter'
    title <- 'Distribució de referències a diaris (top 10)'
    caption <- paste0('Només inclou els comptes twitter següents:\n', the_list)
    
  }
  subtitle <- ifelse(only_one, paste0('', plot_data$newspaper[1]), '')
  y <- 'Percentage*'
  
  if(switch){
    a <- subtitle
    b <- title
    title <- a
    subtitle <- ''
    caption <- ''
  }
  g <- g +
    theme_vilaweb() +
    labs(x = x, y = y, title = title, subtitle = subtitle, caption = caption) +
    theme(plot.subtitle = element_text(size = 24, hjust = 0.5),
          plot.caption = element_text(size = 7)) +
    geom_text(aes(label = round(p, digits = 2)),
              nudge_y = ny,
              alpha = 0.6, size = 3) +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1))
  
  return(g)
}

overall_plot <- function(language = 'en'){
  plot_data <- df %>%
    group_by(newspaper) %>%
    tally %>% ungroup %>%
    mutate(p = n / sum(n) * 100) %>%
    arrange(desc(n))
}

