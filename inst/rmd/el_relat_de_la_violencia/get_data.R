library(vilaweb)
library(rtweet)
library(tidyverse)

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
            'CiudadanosCs',
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
            'enoticiescat'
            )

# Get twitter data
out_list <- list()
for(p in 1:length(people)){
  this_person <- people[p]
  file_name <- paste0('data/', this_person, '_tweets/tweets.csv')
  if(!file.exists(file_name)){
    message(toupper(this_person), '----------------')
    system(paste0("python3 ../../../foreign/twint/Twint.py -u ",
                  this_person,
                  " -o data/",
                  this_person, 
                  "_tweets.csv --csv"))
  }
  # Read in the data
  tl <- read_csv(paste0('data/', this_person, '_tweets/tweets.csv'))
  out_list[[p]] <- tl
}
# Combine all the different users data
tl <- bind_rows(out_list)
