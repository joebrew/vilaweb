library(tidyverse)

# Define function for reading mossos data
mossos_dir <- 'data/mossos/'
mossos_files <- dir(mossos_dir)

read_mosso <- function(file_name){
  file_name = paste0(mossos_dir, mossos_files[1])
  data <- read_delim(file_name, delim = ';', locale = locale(encoding = "latin1"))
  return(data)
}

mosso_list <- list()
for(i in 1:length(mossos_files)){
  this_file <- paste0(mossos_dir, mossos_files[i])
  data <- read_mosso(this_file)
  mosso_list[[i]] <- data
}
