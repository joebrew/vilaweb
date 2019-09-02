library(tidyverse)
eurostat <- read_csv('data/eurostat/crim_off_cat_1_Data.csv')
eurostat_labels <- read_csv('data/eurostat/crim_off_cat_Label.csv')
eurostat_cities <- read_delim('data/eurostat/crim_hom_city.tsv', delim = '\t')

fix_names <- function(x){
  x <- ifelse(grepl('Kosovo', x), 'Kosovo',
              ifelse(grepl('Bosnia', x), 'Bosnia', 
                     ifelse(grepl('Germany', x), 'Germany', x)))
}

# Homicide
make_plot <- function(iccs = 'Intentional homicide',
                      unit = 'Per hundred thousand inhabitants',
                      filter_countries = TRUE,
                      countries = NULL){
  pd <- eurostat %>%
    mutate(GEO = fix_names(GEO)) %>%
    filter(ICCS == iccs,
           UNIT == unit)
  if(filter_countries){
    if(is.null(countries)){
      countries <- c('Denmark', 'England and Wales',
                     'France', 'Ireland', 'Italy', 
                     'Portugal', 'Scotland', 'Spain', 'Norway',
                     'Switzerland', 'Sweden', 'Belgium')
    }
    pd <- pd %>%
      filter(GEO %in% countries)
  }
  ggplot(data = pd,
         aes(x = TIME,
             y = Value)) +
    geom_line() +
    facet_wrap(~GEO, scales = 'free')
}


make_static_plot <- function(iccs = 'Intentional homicide',
                      unit = 'Per hundred thousand inhabitants',
                      filter_countries = TRUE,
                      countries = NULL,
                      add_barcelona = TRUE){
  pd <- eurostat %>%
    mutate(GEO = fix_names(GEO)) %>%
    filter(ICCS == iccs,
           UNIT == unit)
  if(filter_countries){
    if(is.null(countries)){
      countries <- c('Denmark', 'England and Wales',
                     'France', 'Ireland', 'Italy', 
                     'Portugal', 'Scotland', 'Spain', 'Norway',
                     'Switzerland', 'Sweden', 'Belgium')
    }
    pd <- pd %>%
      filter(GEO %in% countries)
  }
  pd <- pd %>%
    mutate(Value = as.numeric(Value)) %>%
    filter(!is.na(Value)) %>%
      group_by(GEO) %>%
      filter(TIME == max(TIME)) 
    pd$is_barcelona <- FALSE
    if(add_barcelona){
      pd <- pd %>% 
        bind_rows(tibble(GEO = 'Barcelona 2019',
                         Value = 1.11,
                         is_barcelona = TRUE))
    }

  ggplot(data = pd,
         aes(x = GEO,
             y = Value)) +
    geom_bar(stat = 'identity',
             aes(fill = is_barcelona)) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    scale_fill_manual(name = '', values = c('grey', 'red')) +
    theme(legend.position = 'none') +
    scale_y_continuous(breaks = seq(0, 10, 0.2)) +
    geom_text(aes(label = round(Value, digits = 2)),
              nudge_y = 0.1)
}
make_static_plot()
make_static_plot(iccs = 'Theft',
                 unit = 'Per hundred thousand inhabitants',
                 filter_countries = TRUE,
                 countries = NULL,
                 add_barcelona = FALSE)
