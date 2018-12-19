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
              # 'enoticiescat',
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
              'ximopuig', 'monicaoltra', 'compromis', 'enricmorera', 'joanribo', 'joanbaldovi', 'F_Armengol', 'adacolau', 'toninoguera', 'isabelbonig', 'vicentsolerm', 'JeanMarcPujol', 'MESperMallorca', 'MesperMenorca', 'SocialistesVAL', 'CatenComu_Podem', 'GabrielRufian_')
  
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

}