make_it <- function(){
  library(devtools)
  library(roxygen2)
  library(rmarkdown)
  library(usethis)
  usethis::use_package('dplyr')
  usethis::use_package('ggmap')
  usethis::use_package('ggplot2')
  usethis::use_package('ggthemes')
  usethis::use_package('leaflet')
  usethis::use_package('lubridate')
  usethis::use_package('maps')
  usethis::use_package('sp')
  usethis::use_package('DT')
  usethis::use_package('Hmisc')
  usethis::use_package('extrafont')
  usethis::use_package('grDevices')
  usethis::use_package('grid')
  usethis::use_package('scales')
  
  document('.')
  install('.')
  render('README.Rmd')
}
make_it()

# setwd('vignettes')
# render('vignette.Rmd')
rebuild <- FALSE
if(rebuild){
  setwd('data-raw')
  source('create_data_files.R')
  setwd('..')
  make_it()
}