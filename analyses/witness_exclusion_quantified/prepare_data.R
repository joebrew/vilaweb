library(readr)
library(tidyverse)

# Read data
data <- read_csv('data/raw_data.csv')

# Clean up a bit
names(data) <-
  c('who', 'group', 'type', 'status', 'by1', 'by2')
data$by <-
  paste0(data$by1, ', ', data$by2)
data$by <- gsub(',', '', data$by)
data$by <- gsub('NA', '', data$by)
data <- data %>%
  dplyr::select(-by1,-by2) %>%
  dplyr::mutate(by = trimws(by))
data <- data %>%
  filter(!is.na(who))

# Quantify number
data$n <- 1
data$n[1:5] <- c(1, 114, 15, 2, 80)

# get list of defendants
requestors <-
  sort(unique(unlist(strsplit(paste0(data$by, collapse = ' '), ' '))))

# Get overall numbers
overall <- data_frame(requestor = requestors,
                      requested = NA,
                      accepted = NA)
for(i in 1:length(requestors)){
  this_requestor <- requestors[i]
  requested
}