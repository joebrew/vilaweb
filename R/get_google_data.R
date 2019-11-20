#' Get google data
#' 
#' Get a table from google sheets
#' @param name The name of the sheet (one of "diputats")
#' @param url The url of the sheet
#' @import gsheet
#' @return a tibble
#' @export

get_google_data <- function(name = c('parlament', 'politicians', 'newspapers', 'socialists', 'congreso'),
                            url = NULL){
  if(is.null(url)){
    if(name == 'parlament'){
      url <- 'https://docs.google.com/spreadsheets/d/1DBKQi5eN9zT_Pj4J3MRiE3qLXB2VPxvd8BVdSc012Ug/edit#gid=0'
    } else if(name == 'politicians'){
      url <- 'https://docs.google.com/spreadsheets/d/1k6_AlqojK47MMqzuFYAzBnDfYXysmUgSseaKvHTb3W4/edit#gid=1425313388'
    } else if(name == 'newspapers'){
      url <- 'https://docs.google.com/spreadsheets/d/1k6_AlqojK47MMqzuFYAzBnDfYXysmUgSseaKvHTb3W4/edit#gid=148951570'
    } else if(name == 'socialists'){
      url <- 'https://docs.google.com/spreadsheets/d/12NwO0huV1Fo8MES5ZHBLZeEbxRwwP36wOleS1zkpPgc/edit#gid=0'
    } else if (name =='congreso'){
      url <- 'https://docs.google.com/spreadsheets/d/1KVyZ-VcG7cc-nnunEli-wmcqJTTbTWJi8w0w_o_1RUk/edit#gid=0'
    }
  } else {
    message('A url was provided. Ignoring name argument.')
  }
  
  
  
  message('Fetching data from ', url)
  data <- gsheet::gsheet2tbl(url = url)
  return(data)
}
