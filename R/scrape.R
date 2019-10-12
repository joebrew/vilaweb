#' Scrape data using twint
#' 
#' Use twint to get data
#' @param search_string A Character string
#' @param file_path Path to file
#' @param start_date The date to start with
#' @param end_date The date to end with
#' @return A csv file
#' @export

scrape <- function(search_string,
                            file_path = 'twitter_data.csv',
                            start_date = NULL,
                            end_date = NULL){
  if(!is.null(start_date)){
    start_string <- paste0(' --since ', as.character(start_date))
  } else {
    start_string <- ''
  }
  if(!is.null(end_date)){
    end_string <- paste0(' --until ', as.character(end_date))
  } else {
    end_string <- ''
  }
  
  # Fix quotations
  search_string <- gsub("'", '"', search_string, fixed = T)
  
  content <- paste0('twint -s "', search_string, '"',
                    start_string,
                    end_string, 
                    ' -o ',
                    file_path,
                    ' --csv')
  message('Running the following in terminal:\n',
          content)
  system(content)
  
  
} 
