# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Get data from socialists
people <- c('sanchezcastejon',
            'miqueliceta',
            'psoe',
            'carmencalvo_')
# update_database(people = people)
tl <- vilaweb::get_twitter_data_from_database(people = people)

# Define the searcher
find_progre <- function(x){
  grepl('derecha', tolower(x))
}

# Search
tl$progre <- find_progre(tl$tweet)

# Get numbers by week
pd <- tl %>%
  filter(date >= '2019-01-01') %>%
  group_by(username, 
           date = as.Date(cut(date, 'month')),
           # date = ifelse(date <= '2019-04-28', 'Before', 'After'),
           progre) %>%
  tally %>%
  group_by(username, date) %>%
  mutate(p = n / sum(n) * 100) %>%
  filter(progre)
left <- expand.grid(username = sort(unique(pd$username)),
                    # date = seq(min(pd$date),
                    #            max(pd$date),
                    #            1)
                    date = sort(unique(pd$date))
                    )
pd <- left_join(left, pd)
pd$n[is.na(pd$n)] <- 0
pd$p[is.na(pd$p)] <- 0
pd <- pd %>% filter(username == 'sanchezcastejon')
ggplot(data = pd,
       aes(x = date,
           y = n,
           group = username)) +
  geom_bar(stat = 'identity') +
  ggthemes::theme_fivethirtyeight() +
  geom_text(aes(label = n),
            nudge_y = -2,
            size = 6,
            color = 'white',
            alpha = 0.6)+
  geom_text(data = pd %>% filter(date >= '2019-06-01'),
                                 aes(label = n),
            nudge_y = 2,
            size = 6,
            color = 'black',
            alpha = 0.6)+
  scale_x_date(breaks = sort(unique(pd$date)),
               labels = format(sort(unique(pd$date)), '%b\n%Y')) +
  theme(axis.text.x = element_text(size = 16
                                   #angle = 90,
                                   #vjust = 1,
                                   #hjust = 0.5
                                   )) +
  labs(x = 'Mes',
       title = 'Tuits de Pedro Sánchez con la palabra "derecha(s)"')
  # facet_wrap(~username, scales = 'free_y')
