#' Get timeline by date
#' 
#' Get a timeline of a twitter user(s), but go back as far as a certain date (as opposed to the typical use case of simply getting the most recent n tweets). Note that the function in many cases retrieves data OLDER than the date argument.
#' @import rtweet
#' @import dplyr
#' @import readr
#' @return A dataframe
#' @export

get_timeline_by_date <- function(user, date, n = 3200){
  final_list <- list()
  for(i in 1:length(user)){
    this_user <- user[i]
    message('Retrieving data for user ', this_user)
    
    
    # Get the timeline
    counter <- 1
    out_list <- list()
    message('Getting first ', n, ' tweets')
    out_list[[counter]] <- old_data <- get_timeline(user = this_user, n = n)
    max_id <- as.character(min(as.numeric(old_data$status_id)))
    min_date <- previous_min_date <- as.Date(min(old_data$created_at))
    while(min_date >= date){
      message('---Data retrieved as far back as ', min_date)
      # Get some new data
      new_data <- get_timeline(user = this_user,
                               max_id = max_id,
                               n = n)
      if(nrow(new_data) == 0){
        break
      }
      # Define a new max_id 
      max_id <- as.character(min(as.numeric(new_data$status_id)))
      # Define a new min date
      min_date <- as.Date(min(new_data$created_at))
      # Add to list
      counter <- counter + 1
      out_list[[counter]] <- new_data
      # If there are no older tweets, break the loop
      if(min_date == previous_min_date){
        message('No older dates')
        break
      } else {
        previous_min_date <- min_date
      }
    }
    message('---Done retrieving for ', this_user)
    this_out <- bind_rows(out_list)
    final_list[[i]] <- this_out
  }
  final <- bind_rows(final_list)
  write_as_csv(final, 'temp.csv')
  final <- readr::read_csv('temp.csv')
  final <- final %>% dplyr::distinct(.keep_all = TRUE)
  return(final)
}
