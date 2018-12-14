for (i in 1:160){
  message(i)
  file <- paste0('DSCD-12-PL-', i, '.PDF')
  download.file(paste0('http://www.congreso.es/public_oficiales/L12/CONG/DS/PL/',
                       file),
                destfile = file)
  Sys.sleep(1)
}
