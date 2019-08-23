
# Get newspaper headlines
if(!dir.exists('newspaper_headlines')){
  dir.create('newspaper_headlines')
}
# Get newspapers
newspapers <- c('elpais', 'abc',
                'elmundo',
                'larazon',
                'lavanguardia',
                'elperiodico')
dates <- seq(as.Date('2019-05-01'), as.Date('2019-08-22'),by = 1)
for(i in 1:length(newspapers)){
  for(j in 1:length(dates)){
    this_newspaper <- newspapers[i]
    this_date <- dates[j]
    formatted_date <- format(this_date, '%Y/%m/%d')
    this_path <- 
      paste0("http://img.kiosko.net/",
             formatted_date,
             "/es/", 
             this_newspaper,
             ".750.jpg")
    message(this_newspaper, ' - ', this_date)
    this_file <- paste0('newspaper_headlines/',
                        this_date,
                        '_',
                        this_newspaper,
                        '.jpg')
    if(!file.exists(this_file)){
      message('...Downloading')
      download.file(url = this_path,
                    destfile =
                      this_file)
      Sys.sleep(1)
    } else {
      message('...Skipping')
    }
  }
}

add_month <- function(x){
  d <- as.Date(x, format = '%Y%m%d00')
  month(d) <- month(d) + 1
  format(d, '%Y%m%d00')
}
