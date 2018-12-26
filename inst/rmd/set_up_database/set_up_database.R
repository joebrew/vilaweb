
library(dplyr)
library(readr)
library(RPostgreSQL)
# # From within psql
# CREATE DATABASE twitter;
# # Now from command line:
# psql twitter

set_up_database <- function(people = NULL){
  
  # If null, do everyone
  if(is.null(people)){
    # people = get from database
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
                'publico_es',
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
    people <- tolower(people)
  }
  
  # Make sure everything in data is lowercase
  setwd('data')
  data_dir <- dir()
  for(i in 1:length(data_dir)){
    old_name <- data_dir[i]
    new_name <- tolower(old_name)
    if(old_name != new_name){
      file.rename(from = old_name, to = new_name)
        # print(old_name)
    }
  }
  setwd('..')
  
  
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
  
  # Write the database
  pg = dbDriver("PostgreSQL")
  con = dbConnect(pg, dbname="twitter")
  dbWriteTable(con,'twitter',tl, row.names=FALSE)
  # Read back
  # dtab = dbGetQuery(con, "select * from twitter")
  # disconnect from the database
  dbDisconnect(con)
  
}
