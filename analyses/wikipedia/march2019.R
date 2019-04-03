library(tidyverse)
data <- dir('march2019')
out_list <- list()
for(i in 1:length(data)){
  this_person <- data[i]
  this_file <- paste0('march2019/',
                      this_person)
  out <- read_csv(this_file) %>%
    mutate(person = 
             Hmisc::capitalize(gsub('langviews-', '', this_person, fixed = TRUE)))
  out_list[[i]] <- out
}
df <- bind_rows(out_list) %>%
  mutate(person = gsub('.csv', '', person, fixed = TRUE))

df <- df %>% gather(key, value,
                    `2019-03-01`:`2019-03-31`)
lang_dict <- tibble(
  Language = c('en', 'ca', 'es', 'fr'),
  language = c('English',
               'Català',
               'Español',
               'Français')
)

df <- df %>%
  group_by(person) %>%
  mutate(Title = dplyr::first(Title)) %>%
  ungroup %>%
  mutate(person = Title) %>%
  dplyr::select(-Title, -Badges) %>%
  filter(Language %in% c('en', 'ca', 'es', 'fr')) %>%
  left_join(lang_dict) %>%
  dplyr::select(-Language)


person_dict <- tibble(
  Person = c('Albert Rivera',
             'Carles Puigdemont',
             'Inés Arrimadas',
             'Oriol Junqueras',
             'Pablo Casado',
             'Pablo Iglesias',
             'Santiago Abascal',
             'Jordi Cuixart',
             'Quim Torra',
             'Manuel Valls'),
  person = c('Albert Rivera',
             'Carles Puigdemont i Casamajó',
             'Inés Arrimadas',
             'Oriol Junqueras i Vies',
             'Pablo Casado',
             'Pablo Iglesias Turrión',
             'Santiago Abascal Conde',
             'Jordi Cuixart i Navarro',
             'Joaquim Torra i Pla',
             'Manuel Valls (político)')
)

agg <- df %>%
  left_join(person_dict) %>%
  mutate(Person = ifelse(is.na(Person), person, Person)) %>%
  group_by(language, person = Person) %>%
  summarise(value = sum(value)) %>%
  filter(person %in%
           c('Albert Rivera',
             'Carles Puigdemont',
             'Inés Arrimadas',
             'Oriol Junqueras',
             'Pablo Casado',
             'Pablo Iglesias',
             'Santiago Abascal',
             'Pedro Sánchez',
             # 'Jordi Cuixart',
             # 'Manuel Valls',
             'Quim Torra',
             'Miquel Iceta',
             # 'Roger Torrent',
             'Pere Aragonès'))

agg$person <- unlist(lapply(strsplit(agg$person, ' '), function(x){x[2]}))

library(ggplot2)
library(databrew)
ggplot(data = agg,
       aes(x = person,
           y = value)) +
  geom_bar(stat = 'identity',
           aes(fill = person),
           position = position_dodge()) + 
  facet_wrap(~language,
             scales = 'free_y'
             ) +
  theme_databrew()+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5)) +
  scale_fill_manual(name = '',
                    values = databrew::make_colors(n = length(unique(agg$person)))) +
  theme(legend.position = 'none') +
  labs(x = '',
       y = 'Views',
       title = 'March 2019 Wikipedia page views',
       caption = 'Data from Wikipedia (retrieved on 3 April 2019 via https://tools.wmflabs.org/langviews).\nChart by Joe Brew | @joethebrew..\n') +
  theme(plot.title = element_text(size = 28),
        strip.text = element_text(size = 26),
        axis.text.x = element_text(size = 18))
ggsave('~/Desktop/chart.png')
